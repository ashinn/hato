;; hato-log.scm -- Apache-style logging levels
;;
;; Copyright (c) 2005-2008 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; (cond-expand
;;  ((and chicken compiling)
;;   (declare
;;    (export
;;     current-log-level
;;     log-open log-close log-display log-format
;;     log-emergency log-alert log-critical log-error
;;     log-warn log-notice log-info log-debug)))
;;  (else))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-log-level 6)  ; default to info
(define current-log-port (current-error-port))
(define current-log-file #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (define-logger logger level)
  (let* ((t (symbol->string logger))
         (type (if (and (> (string-length t) 4)
                        (string=? "log-" (substring t 0 4)))
                   (substring t 4)
                   t)))
    `(define-macro (,logger fmt . o)
       `(if (>= current-log-level ,,level)
            (log-format ',,type ,fmt ,@o)))))

;;             Level          Example
;;             -----          -------
(define-logger log-emergency 0) ; the server is on fire!!!
(define-logger log-alert     1) ; couldn't write to user mailbox
(define-logger log-critical  2) ; couldn't run 'dig' executable
(define-logger log-error     3) ; error loading user filter
(define-logger log-warn      4) ; invalid smtp command; relay failed
(define-logger log-notice    5) ; saved to file/relayed to address
(define-logger log-info      6) ; loaded alias file
(define-logger log-debug     7) ; spam-probability: 0.5

(define-constant *week-day-abbrevs*
  (vector "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

(define (current-log-date-string)
  (define (pad0 n len)
    (string-pad (number->string n) len #\0))
  (define (week-day-abbrev i)
    (if (<= 0 i 6) (vector-ref *week-day-abbrevs* i) "___"))
  (let ((now (seconds->local-time (current-seconds))))
    ;; this makes lexicographic sort == chronological sort
    (sprintf "~A-~A-~A ~A:~A:~A"
             (pad0 (+ 1900 (vector-ref now 5)) 4)
             (pad0 (+ 1 (vector-ref now 4)) 2)
             (pad0 (vector-ref now 3) 2)
             (pad0 (vector-ref now 2) 2)
             (pad0 (vector-ref now 1) 2)
             (pad0 (vector-ref now 0) 2))))

;; Use file-locking to let multiple processes write to the same log
;; file.  On error try to re-open the log file.  We keep the port open
;; so that even if you mv the file (e.g. when rotating logs) we keep
;; writing to it in the new location.  To force writing to a new file in
;; the original location, use cp+rm instead of mv, or alternately SIGHUP
;; the process after the mv.
(define (log-display type str)
  (let* ((prefix (sprintf "[~A] [~A] " (current-log-date-string) type))
         (str (string-append prefix ; prefix all lines in message
                             (string-intersperse (string-split str "\n")
                                                 (string-append "\n" prefix))
                             "\n")))
    (condition-case
     (let ((lock (and (output-port? current-log-port)
                      (file-lock current-log-port))))
       ;; this is redundant with POSIX O_APPEND
       ;;(set-file-position! current-log-port 0 seek/end)
       (display str current-log-port)
       (flush-output current-log-port)
       (and lock (file-unlock lock)))
     (e1 () ; try to re-open log-file, use stderr as backup
      (condition-case
       (begin
         (log-close)
         (log-open)
         (let ((lock (file-lock current-log-port)))
           (display str current-log-port)
           (flush-output current-log-port)
           (file-unlock lock)))
       (e2 ()
         (let ((err (current-error-port)))
           (if (and (output-port? err)
                    (not (eq? err current-log-port)))
               (display str err)))))))))

(define (log-format type fmt . args)
  (log-display type (apply sprintf fmt args)))

(define (log-open . o)
  (if (pair? o) (set! current-log-file (car o)))
  (if current-log-file
      (set! current-log-port (open-output-file current-log-file append:))))

(define (log-close)
  (if (output-port? current-log-port)
      (close-output-port current-log-port)))

(define (log-error-message exn)
  (log-error "~A" (call-with-output-string
                    (lambda (out) (print-error-message exn out "")))))

(define (log-call-chain . o)
  (let ((call-chain (if (pair? o) (car o) (get-call-chain 1 #t))))
    (for-each
     (lambda (info) 
       (let ((more1 (##sys#slot info 1))
	     (more2 (##sys#slot info 2)))
         (fprintf current-log-port "\t~A\t\t" (##sys#slot info 0))
	 (if more2 (fprintf current-log-port "[~A] " more2))
	 (if more1
             (##sys#with-print-length-limit
              100
              (lambda () (##sys#print more1 #t current-log-port))))
         (newline current-log-port)))
     call-chain)))

(define (log-error&call-chain exn)
  (let ((call-chain (get-call-chain 1 #t)))
    (log-error-message exn)
    (log-call-chain call-chain)))

