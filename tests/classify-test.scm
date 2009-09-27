;;;; test-classify.scm -- sanity checks for the classifier
;;
;; Copyright (c) 2005 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; Change these to the locations of a ham-message and spam-message
;; accordingly.
(define m1 "./data/ham/000/00001.7c53336b37003a9286aba55d2945844c")
(define m2 "./data/spam/000/00001.7848dde101aa985090474a91ec93fcf0")

(use srfi-1 format regex posix utils)

(define prob-rx
  (regexp "^(ham|spam) *([^ ]+)" #t))

(define (=~ a b . o)
  (< (abs (- a b)) (if (pair? o) (car o) 0.1)))

(define *pass* 0)
(define *fail* 0)

(define-macro (test form . opt)
  (let ((res (gensym))
        (expect (gensym))
        (expect-value (if (pair? opt) (car opt) #t)))
    `(let ((,res ,form) (,expect ,expect-value))
       (if (equal? ,res ,expect)
         (begin
           (set! *pass* (+ 1 *pass*))
           (display "[ OK ] ") (write ',form) (display " => ") (write ,res) (newline))
         (begin
           (set! *fail* (+ 1 *fail*))
           (display "[FAIL] ") (write ',form) (display " => ") (write ,res)
           (display " [expected ") (write ,expect) (display "]") (newline))))))

(define (test-command command)
  (define (run-prob command)
    (with-input-from-pipe
     command
     (lambda ()
       (let lp ((prev #f))
         (let ((line (read-line)))
           (if (eof-object? line)
             (and-let* (((string? prev))
                        (m (string-match prob-rx prev)))
               (string->number (caddr m)))
             (lp line)))))))
  ;; tests:
  ;;   1. train 1 as ham should send probability to 0.0
  ;;   2. check again in verbose mode should still be 0.0
  ;;   3. train another as spam should send probability to near 1.0
  ;;   4. check again in verbose mode should be same
  ;;   5. check first ham again should still be ham
  (test (zero? (system (string-append command " -ham " m1))))
  (let* ((h1 (run-prob (string-append command " -p " m1)))
         (h2 (run-prob (string-append command " -p -verbose " m1))))
    (test (=~ h1 0.0))
    (test (=~ h2 0.0))
    (test (=~ h1 h2 0.01))
    (test (zero? (system (string-append command " -spam " m2))))
    (let* ((s1 (run-prob (string-append command " -p " m2)))
           (s2 (run-prob (string-append command " -p -verbose " m2))))
      (test (=~ s1 1.0))
      (test (=~ s2 1.0))
      (test (=~ s1 s2 0.01))
      (let ((h3 (run-prob (string-append command " -p " m1))))
        (test (=~ h3 0.0 0.3))
        (printf "h1: ~S h2: ~S h3: ~S s1: ~S s2: ~S\n" h1 h2 h3 s1 s2)
        ))))

(define (cartesian-product-for-each proc lol)
  (if (null? lol)
    (proc '())
    (for-each
     (lambda (x)
       (cartesian-product-for-each
        (lambda (sub-prod)
          (proc (cons x sub-prod)))
        (cdr lol)))
     (car lol))))

(cartesian-product-for-each
 (lambda (ls)
   (apply
    (lambda (folders parsers algorithm key-size value-size chain-length
        min-length weight-factor num-significant epsilon)
      (let ((command
             (sprintf "../hato-classify ~A ~A ~A -k ~A -v ~A -c ~A -m ~A -w ~A -n ~A -epsilon ~A "
                      (string-intersperse folders)
                      (string-intersperse parsers)
                      (string-intersperse algorithm)
                      key-size
                      value-size
                      chain-length
                      min-length
                      weight-factor
                      num-significant
                      epsilon)))
        (printf "running: ~A\n" command)
        (system (string-append command "-delete-database"))
        (let ((start (current-milliseconds)))
          (receive (user0 sys0) (cpu-time)
            (test-command command)
            (let ((end (current-milliseconds)))
              (receive (user1 sys1) (cpu-time)
                (printf "~Sms user ~Sms system ~Sms real\n"
                        (- user1 user0) (- sys1 sys0) (- end start))))))
        (system (string-append command "-delete-database"))
        ))
    ls))
 ;; (* 2 4 2 2 5 2 3 3 5) => 14400 (# of minutes in a day)
 '((("-literal")      ; folders
    ("-case-insensitive")
    ("-deleet")
    ("-case-insensitive" "-deleet" "-literal"))
   (()
    ("-mime")
    ("-html"))
   (()
    ("-robinson")
    ("-naive-bayes"))
   (3)                  ; key-size (2, 4)
   (2 4)                  ; value-size
   (5)                ; chain-size
   (1)                  ; min-length
   (0)                ; weight factor
   (0)             ; num-significant
   (0.0 0.2)  ; epsilon
   ))

(display "Tests complete.") (newline)
(display "Pass: ") (display *pass*) (newline)
(display "Fail: ") (display *fail*) (newline)

