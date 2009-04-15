#!/bin/sh
#|
exec csi -s "$0" "$@"
|#

;; This script is designed for use when you have many extensions in
;; the same directory.  Currently, Chicken will complain about those
;; extensions not being installed yet (unless you install them, which
;; messes with testing), and moreover if you use -G you'll get a
;; warning for every imported variable you refer to.

;; So we just filter out all those warnings when they refer to an
;; extension available in the current directory, or a variable
;; imported from such.

(use regex posix utils srfi-1)

(define (extract-source-exports sexp)
  (if (not (pair? sexp))
      '()
      (case (car sexp)
        ((export)
         (filter symbol? (cdr sexp)))
        ((begin declare)
         (append-map extract-source-exports (cdr sexp)))
        ((cond-expand)
         (append-map (lambda (x)
                       (append-map extract-source-exports (cdr x)))
                     (cdr sexp)))
        (else
         '()))))

(define (extract-local-exports mod)
  (let ((exports-file (conc mod ".exports"))
        (source-file (conc mod ".scm")))
    (cond
     ((file-exists? exports-file)
      (with-input-from-file exports-file
        (lambda () (port-map string->symbol read-line))))
     ((file-exists? source-file)
      (let ((top-level
             (with-input-from-file source-file
               (lambda () (port-map identity read)))))
        (append-map extract-source-exports top-level)))
     (else '()))))

(define missing-extension-rx
  (regexp "^Warning: extension `([^`' ]+)' is currently not installed" #t))

(define undefined-variable-rx
  (regexp "^Warning: variable `([^`' ]+)' used but not imported" #t))

(let lp ((imports '()))
  (let ((line (read-line)))
    (cond
     ((eof-object? line))
     ((string-match "^csc .*" line)
      => (lambda (m)
           (print line)
           (lp '())))
     ((string-match missing-extension-rx line)
      => (lambda (m)
           (cond
            ((file-exists? (string-append (cadr m) ".scm"))
             (lp (append (extract-local-exports (string->symbol (cadr m)))
                         imports)))
            (else
             (print line)
             (lp imports)))))
     ((string-match undefined-variable-rx line)
      => (lambda (m)
           (if (not (memq (string->symbol (cadr m)) imports))
               (print line))
           (lp imports)))
     (else
      (print line)
      (lp imports)))))

