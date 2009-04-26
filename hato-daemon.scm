;; hato-daemon.scm -- utility to run a daemon
;;
;; Copyright (c) 2008-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use extras posix regex tcp tcp-server)

(module hato-daemon
  (running-process-id? daemonize daemon-kill)

(import scheme chicken extras ports posix regex foreign srfi-18 tcp tcp-server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
          (and (file-exists? file)
               (with-input-from-file file read-line)))
      (exn () #f))))

 (else

  ;; use sysctl
  (foreign-declare "#include <sys/sysctl.h>")
  (foreign-declare "#include <sys/proc.h>")

  (define pid->cmdline
    (foreign-lambda* c-string ((int pid))
      "unsigned long reslen = sizeof(struct kinfo_proc);
       struct kinfo_proc* res = malloc(reslen);
       int name[4] = {CTL_KERN, KERN_PROC, KERN_PROC_PID, pid};
       if (sysctl(name, 4, res, &reslen, NULL, 0) >= 0) {
         C_return(res->kp_proc.p_comm);
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

(define (daemon-kill pid-file
                     #!key
                     (name #f)
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
      ;; maybe clean up the pid file
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
      (warner "no process to kill")))))

(define (daemonize #!key name pid-file user-id group-id
                   running-handler stale-handler invalid-handler
                   root-thunk init-thunk tcp-port tcp-backlog tcp-host
                   tcp-handler tcp-max-requests tcp-debug?)

  ;; verify there's no process already running
  (if (and pid-file (file-exists? pid-file))
      (let ((pid (string->number (call-with-input-file pid-file read-line))))
        (cond
         ((not (number? pid))
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
              (error "stale pid file" pid-file))))))

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
          ((make-tcp-server listener tcp-handler (or tcp-max-requests 100))
           tcp-debug?)
          (if tcp-port
              (error "couldn't bind to port" tcp-port)
              (error "tcp-handler requires a tcp-port argument"))))
     ;; just return the listener if a handler isn't specified
     (listener))))

)
