;;;; run-compare.scm -- run comparison of classifier with different settings
;;
;; Copyright (c) 2005 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(use srfi-1 format regex posix utils)

(define (run-command command train-every?)
  (define (run option dir failed? message)
    (if (file-exists? dir)
      (let lp ((ls (cddr (directory dir))) (res 0))
        (if (null? ls)
          res
          (if (failed? (system (string-append command " -e " dir (car ls))))
            (begin ; retrain
              (print "FALSE " message " " (car ls))
              (system (string-append command " -" option " -refile "
                                     " -auto-learn " dir (car ls)))
              (lp (cdr ls) (+ res 1)))
            (begin
              (if train-every?
                (system (string-append command " -" option " " dir (car ls))))
              (lp (cdr ls) res)))))
      0))
  (let lp ((i 0) (j 0) (false-pos 0) (false-neg 0))
    (if (>= i 42)
      (list false-pos false-neg)
      (let* ((fn1 (run "spam" (format"./data/spam/~3'0D/"j) zero? "NEGATIVE"))
             (fn2 (run "spam" (format"./data/spam/~3'0D/"(+ j 1)) zero? "NEGATIVE"))
             (fp (run "ham" (format"./data/ham/~3'0D/"i) positive? "POSITIVE")))
        (lp (+ i 1) (+ j 2) (+ false-pos fp) (+ false-neg fn1 fn2))))))

(define (folder-char x)
  (cond
    ((equal? x "-literal") #\s)
    ((equal? x "-case-insensitive") #\i)
    ((equal? x "-deleet") #\1)
    (else #\x)))

(define (parser-char x)
  (cond
    ((equal? x "-html") #\h)
    ((equal? x "-mime") #\m)
    (else #\x)))

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
    (lambda (train-every? folders parsers algorithm key-size value-size chain-length
        min-length weight-factor num-significant epsilon)
      (let ((command
             (sprintf "./hato-classify ~A ~A ~A -k ~A -v ~A -c ~A -m ~A -w ~A -n ~A -epsilon ~A "
                      (string-intersperse folders)
                      (string-intersperse parsers)
                      (string-intersperse algorithm)
                      key-size
                      value-size
                      chain-length
                      min-length
                      weight-factor
                      num-significant
                      epsilon))
            (report-file
             (sprintf "spam-~A-~A-~A-~A-k~A-v~A-c~A-m~A-w~A-n~A-e~A.log"
                      (if train-every? "te" "toe")
                      (list->string (map folder-char folders))
                      (if (pair? parsers)
                        (list->string (map parser-char parsers))
                        "s")
                      (if (pair? algorithm)
                        (list->string (map (cut string-ref <> 1) algorithm))
                        "c")
                      key-size
                      value-size
                      chain-length
                      min-length
                      weight-factor
                      num-significant
                      epsilon)))
        (fprintf (current-error-port) "running: ~A\n" report-file)
        (fprintf (current-error-port) "running: ~A\n" command)
        (system (string-append command "-delete-database"))
        (with-output-to-file report-file
          (lambda ()
            (let ((start (current-milliseconds)))
              (receive (user0 sys0) (cpu-time)
                (print (run-command command train-every?))
                (let ((end (current-milliseconds)))
                  (receive (user1 sys1) (cpu-time)
                    (printf "~Sms user ~Sms system ~Sms real\n"
                            (- user1 user0) (- sys1 sys0) (- end start))))))))
        (system (string-append command "-delete-database"))
        ))
    ls))
 ;; (* 2 4 2 2 5 2 3 3 5) => 14400 (# of minutes in a day)
 '((#f #t)            ; train-every?
   (("-literal")      ; folders
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


