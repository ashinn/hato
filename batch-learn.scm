;;;; batch-learn.scm -- train until no errors
;;
;; Copyright (c) 2005 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(use srfi-1 format regex posix utils)

(define (train command spam-files ham-files)
  (define (run-spam file)
    (if (positive? (system (string-append command " -spam " file))) 0 1))
  (define (run-ham file)
    (if (positive? (system (string-append command " -ham " file)))
      (begin
        (fprintf (current-error-port) "false pos: ~S\n" file)
        1)
      0))
  (if (or (null? spam-files) (null? ham-files))
    (print "you must specify both ham and spam files")
    (let* ((num-spam (length spam-files))
           (num-ham (length ham-files))
           (ratio (inexact->exact (floor (* 100 (/ num-spam (+ num-spam num-ham))))))
           (spam-files (shuffle spam-files))
           (ham-files (shuffle ham-files)))
      (printf "command: ~S ratio: ~S\n" command ratio)
      (let lp ((spam spam-files) (ham ham-files) (fp 0) (fn 0) (total 0))
        (if (or (null? ham) (zero? total)
                (and (not (null? spam)) (< (random 100) ratio)))
          (if (null? spam)
            (begin
              (printf "~S - ~S false positives, ~S false negatives out of ~S total\n"
                      (/ (- total (+ fp fn)) total) fp fn total)
              (unless (and (zero? fp) (zero? fn))
                (lp spam-files ham-files 0 0 0)))
            (lp (cdr spam) ham fp (+ fn (run-spam (car spam))) (+ total 1)))
          (lp spam (cdr ham) (+ fp (run-ham (car ham))) fn (+ total 1)))))))

(define (mail-files file)
  (cond ((directory? file)
         (append-map mail-files (map (cut string-append file "/" <>)
                                     (cddr (directory file)))))
        ((file-exists? file)
         (list file))
        (else
         (fprintf (current-error-port) "ignoring non-existant file: ~S\n" file)
         (list '()))))

(define (usage)
  (print "usage: learn [options...] -spam <spam-files> ... -ham <ham-files> ...")
  (exit 0))

(define (main args)
  (let lp ((ls args) (pass-args '()) (spam-files '()) (ham-files '()) (spam? #f))
    (if (null? ls)
      (train (string-append (if (file-exists? "./hato-classify")
                              "./hato-classify -e " "hato-classify -e ")
                            (string-intersperse (reverse pass-args)))
             spam-files ham-files)
      (if (and (> (string-length (car ls)) 1)
               (eq? #\- (string-ref (car ls) 0)))
        (case (string->symbol
               (substring (car ls)
                          (if (eq? #\- (string-ref (car ls) 1)) 2 1)))
          ((h help) (usage))
          ((spam) (lp (cdr ls) pass-args spam-files ham-files #t))
          ((ham) (lp (cdr ls) pass-args spam-files ham-files #f))
          ((literal case-insensitive deleet verbose p print-result)
           (lp (cdr ls) (cons (car ls) pass-args) spam-files ham-files spam?))
          (else
           (lp (cddr ls) (cons (cadr ls) (cons (car ls) pass-args)) spam-files ham-files spam?)))
        (let ((files (mail-files (car ls))))
          (if spam?
            (lp (cdr ls) pass-args (append files spam-files) ham-files spam?)
            (lp (cdr ls) pass-args spam-files (append files ham-files) spam?)))))))

(main (command-line-arguments))

