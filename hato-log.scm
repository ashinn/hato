;; hato-log.scm -- Apache-style logging levels
;;
;; Copyright (c) 2005-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(require-library posix)

(module hato-log
  (define-logger log-open log-close log-display log-format
   log-error-message log-call-chain log-error&call-chain)

(import scheme chicken extras data-structures ports posix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-log-port (current-error-port))
(define current-log-file #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-logger
  (er-macro-transformer
   (lambda (expr r c)
     (let* ((level-info (cadr expr))
            (loggers
             (let lp ((ls (caddr expr))
                      (i 0)
                      (res '()))
               (cond ((null? ls)
                      (reverse res))
                     ((pair? (car ls))
                      (if (<= (cadar ls) i)
                          (error "log levels must be increasing" (caddr expr))
                          (lp (cdr ls) (+ (cadar ls) 1) (cons (car ls) res))))
                     (else
                      (lp (cdr ls) (+ i 1) (cons (list (car ls) i) res))))))
            (log-descriptions
             (map (lambda (x)
                    (cons
                     (string->symbol
                      (if (pair? (cddr x))
                          (caddr x)
                          (let ((desc (symbol->string (car x))))
                            (if (and (> (string-length desc) 4)
                                     (string=? "log-" (substring desc 0 4)))
                                (substring desc 4)
                                desc))))
                     (cadr x)))
                  loggers))
            (level-getter (car level-info))
            (level-setter (cadr level-info))
            (default-level (if (pair? (cddr level-info))
                               (caddr level-info)
                               (cadr (car (reverse loggers)))))
            (level-var (r '*log-level*))
            (level-indexer (r 'x->level))
            (_define (r 'define))
            (x (r 'x)))
       `(,(r 'begin)
         (,_define (,level-indexer ,x)
                   (,(r 'cond)
                    ((,(r 'assq) ,x ',log-descriptions)
                     ,(r '=>) ,(r 'cdr))
                    ((,(r 'integer?) ,x) ,x)
                    (,(r 'else) (,(r' error) "invalid log level" ,x))))
         (,_define ,level-var
                   (,(r 'cons) (,level-indexer ',default-level) '()))
         (,_define (,level-getter) (,(r 'car) ,level-var))
         (,_define (,level-setter ,x)
                   (,(r 'set-car!) ,level-var (,level-indexer ,x)))
         ,@(map
            (lambda (logger description)
              (let ((name (car logger))
                    (level (cadr logger)))
                `(,(r 'define-syntax) ,name
                  (,(r 'er-macro-transformer)
                   (,(r 'lambda) (,x ,(r 'r2) ,(r 'c2))
                    (,(r 'quasiquote)
                     ((,(r 'unquote) (,(r 'r2) 'if))
                      ((,(r 'unquote) (,(r 'r2) '>=))
                       (,(r 'car) ,level-var)
                       ,level)
                      ((,(r 'unquote) (,(r 'r2) 'log-format))
                       ,(symbol->string (car description))
                       (,(r 'unquote-splicing) (,(r 'cdr) ,x))))))))))
            loggers
            log-descriptions))))))

;;(define-constant *week-day-abbrevs*
;;  (vector "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

;;(define (week-day-abbrev i)
;;  (if (<= 0 i 6) (vector-ref *week-day-abbrevs* i) "___"))

(define (current-log-date-string)
  (define (pad0 n len)
    (let* ((s (number->string n))
           (diff (- len (string-length s))))
      (if (> diff 0) (string-append s (make-string diff #\0)) s)))
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

(define (log-error-message type exn)
  (log-format type "~A" (call-with-output-string
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

(define (log-error&call-chain type exn)
  (let ((call-chain (get-call-chain 1 #t)))
    (log-error-message type exn)
    (log-call-chain call-chain)))

)

