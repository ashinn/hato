;; hato-daemon.scm -- utility to run a daemon
;;
;; Copyright (c) 2008-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-library extras posix regex tcp let-keywords)

(module hato-daemon
(pid->cmdline running-process-id? run-tcp-server daemonize daemon-kill)

(import scheme chicken extras ports)
(import posix regex foreign srfi-18 tcp let-keywords)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; process utils

(eval-when (compile)
  (if (file-exists? "/proc/1/cmdline")
      (register-feature! 'proc-filesystem)))

(cond-expand
 (proc-filesystem

  ;; use the proc filesystem
  (define (process-id-cmdline-file pid)
    (string-append "/proc/" (number->string pid) "/cmdline"))

  (define (pid->cmdline pid)
    (condition-case
        (let ((file (process-id-cmdline-file pid)))
          (with-input-from-file file read-line))
      (exn () #f))))

 (else

  ;; use sysctl
  (foreign-declare "#include <sys/sysctl.h>")
  (foreign-declare "#include <sys/proc.h>")

  (define pid->cmdline
    (foreign-lambda* c-string ((int pid))
      "unsigned long reslen = sizeof(struct kinfo_proc);
       struct kinfo_proc res;
       int name[4] = {CTL_KERN, KERN_PROC, KERN_PROC_PID, pid};
       if (sysctl(name, 4, &res, &reslen, NULL, 0) >= 0) {
         C_return(res.kp_proc.p_comm);
       } else {
         C_return(NULL);
       }"))
  ))

(define (running-process-id? pid . o)
  (condition-case
      (let ((cmdline (pid->cmdline pid)))
        (and cmdline
             (or (null? o)
                 (not (car o))
                 (string-search (car o) cmdline))))
    (exn () #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tcp utils

(define default-max-requests 10000)

(define (run-tcp-server listener-or-port handler . o)
  (let ((listener (if (integer? listener-or-port)
                      (tcp-listen listener-or-port)
                      listener-or-port)))
    (let-keywords* o
        ((max-requests default-max-requests)
         (accept tcp-accept)
         (debug #f)
         (log-error
          (lambda (msg . args) (apply fprintf (current-error-port) msg args))))
      (define log-debug
        (if (and debug (not (procedure? debug)))
            (lambda (msg . args) (apply fprintf (current-error-port) msg args))
            debug))
      (define run
        (if log-debug
            (lambda (count)
              (log-debug "tcp-server: accepting request ~S from thread ~S\n"
                         count (thread-name (current-thread)))
              (handler)
              (log-debug "tcp-server: finished ~S\n"
                         count (thread-name (current-thread))))
            (lambda (count) (handler))))
      (let ((max-requests (if (integer? max-requests)
                              max-requests
                              default-max-requests)))
        (let serve ((count 0))
          (cond
           ((<= max-requests 0)
            (thread-yield!))
           (else
            (receive (in out) (accept listener)
              (thread-start!
               (make-thread
                (lambda ()
                  (condition-case
                      (begin
                        (set! max-requests (- max-requests 1))
                        (current-input-port in)
                        (current-output-port out)
                        (run count))
                    (exn ()
                         (cond
                          (log-error
                           (log-error "tcp-server: error in thread ~S: ~A\n"
                                      (thread-name (current-thread))
                                      (with-output-to-string
                                        (lambda () (print-error-message exn))))
                           (print-call-chain (current-error-port))))))
                  (close-input-port in)
                  (close-output-port out)
                  (set! max-requests (+ max-requests 1))))))
            (serve (+ count 1)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; daemons

(define (daemonize . o)

  (let-keywords* o
      (name pid-file user-id group-id
       running-handler stale-handler invalid-handler
       root-thunk init-thunk tcp-port tcp-backlog tcp-host
       tcp-handler tcp-max-requests tcp-debug?)

    ;; verify there's no process already running
    (if (and pid-file (file-exists? pid-file))
        (let ((pid (string->number (call-with-input-file pid-file read-line))))
          (cond
           ((not (integer? pid))
            (if invalid-handler
                (invalid-handler)
                (error "invalid pid file" pid-file)))
           ((running-process-id? pid name)
            (if running-handler
                (running-handler)
                (error "process already running" pid)))
           (else
            (if stale-handler
                (stale-handler)
                ;;(error "stale pid file" pid-file)
                (delete-file pid-file))))))

    ;; fork the process and create a new session
    (if (zero? (process-fork))
        (create-session)
        (exit 0))

    ;; bind the tcp port and do anything needed as root
    (if root-thunk (root-thunk))
    (let ((listener (and tcp-port
                         (tcp-listen tcp-port (or tcp-backlog 4) tcp-host))))

      ;; now switch the user and group
      (cond
       ((zero? (current-user-id))
        (if group-id (set! (current-group-id) group-id))
        (if user-id (set! (current-user-id) user-id))))

      ;; log the pid file
      (if pid-file
          (call-with-output-file pid-file
            (lambda (out) (write (current-process-id) out))))

      ;; run any inits
      (if init-thunk (init-thunk))

      ;; maybe run the tcp-server
      (cond
       (tcp-handler
        (if listener
            (run-tcp-server listener tcp-handler
                            'max-requests: tcp-max-requests
                            'debug: tcp-debug?)
            (if tcp-port
                (error "couldn't bind to port" tcp-port)
                (error "tcp-handler requires a tcp-port argument"))))
       ;; just return the listener if a handler isn't specified
       (listener)))))

(define (daemon-kill pid-file . o)
  (let-keywords* o ((name #f)
                    (notifier (lambda o #f))
                    (warner notifier)
                    (wait-limit 5))
    (let ((pid (and (file-exists? pid-file)
                    (condition-case
                        (call-with-input-file pid-file read)
                      (exn () #f)))))
      (cond
       ((and (number? pid) (running-process-id? pid name))
        ;; kill the process
        (notifier "shutting down server with PID: ~A from PID ~A"
                  pid (current-process-id))
        (condition-case
            (process-signal pid)
          (exn ()
               (warner "couldn't SIGTERM process ~A, trying SIGKILL" pid)
               (condition-case
                   (process-signal pid signal/kill)
                 (exn () (warner "couldn't SIGKILL process")))))
        ;; maybe clean up the pid file if the killed process didn't
        (let lp ((i 1))
          (if (file-exists? pid-file)
              (cond
               ((> i wait-limit)
                (warner "couldn't delete pid-file ~A" pid-file))
               ((not (running-process-id? pid name))
                (condition-case (delete-file pid-file)
                  (exn () (warner "error deleting pid-file ~A" pid-file))))
               (else
                (thread-sleep! (* i 0.5))
                (lp (+ i 1)))))))
       ((number? pid)
        (notifier "removing stale lock file for pid ~A" pid)
        (condition-case (delete-file pid-file)
          (exn ()
               (warner "couldn't remove stale lock file: ~A"
                       (call-with-output-string
                        (lambda (out)
                          (print-error-message exn out "")))))))
       (else
        (warner "no process to kill"))))))

)
