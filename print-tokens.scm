;;;; print-tokens.scm -- just print out the tokens from a source
;;
;; Copyright (c) 2003-2005 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(use srfi-1 format posix utils hato-prob hato-mime)

(define (mail-files file)
  (cond
    ((directory? file)
     (append-map mail-files (map (cut string-append file "/" <>)
                                 (cddr (directory file)))))
    ((file-exists? file)
     (list file))
    (else
     (fprintf (current-error-port) "ignoring non-existant file: ~S\n" file)
     (list '()))))

(define (run-literal)
  (let loop ()
    (let ((line (read-line)))
      (unless (eof-object? line)
        (token-fold line '() (lambda (s i j a) (print (substring s i j))))
        (loop)))))

(define (run-mime)
  (mime-message-fold (current-input-port)
                     (mime-headers->list)
                     (lambda (type headers body acc)
                       (token-fold body '()
                                   (lambda (s i j a) (print (substring s i j)))))
                     '()))

(define (run runner args)
  (if (pair? args)
    (for-each (cut with-input-from-file <> runner)
              (append-map mail-files args))
    (runner)))

(define (main args)
  (if (and (pair? args) (string=? "-mime" (car args)))
    (run run-mime (cdr args))
    (run run-literal args)))

(main (command-line-arguments))

