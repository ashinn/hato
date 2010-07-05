;; let-keywords.scm -- portable syntax for keyword parameters
;;
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(module let-keywords (let-keywords*)

(import scheme chicken)

(define-syntax let-keys-error
  (syntax-rules () ((let-keys-error) #f)))

(define-syntax let-keys*
  (syntax-rules ()
    ((let-keys* ls (binds ...) ((var keyword default) . rest) . body)
     (let-keys* ls (binds ... (var keyword default)) rest . body))
    ((let-keys* ls (binds ...) ((var default) . rest) . body)
     (let-keys* ls (binds ... (var var default)) rest . body))
    ((let-keys* ls (binds ...) ((var) . rest) . body)
     (let-keys* ls (binds ... (var var #f)) rest . body))
    ((let-keys* ls (binds ...) (var . rest) . body)
     (let-keys* ls (binds ... (var var #f)) rest . body))
    ((let-keys* ls ((var keyword default) ...) () . body)
     (let ((tmp ls))
       (let* ((var (cond ((memq (string->keyword (symbol->string 'keyword)) ls)
                          => cadr)
                         (else default)))
              ...)
         . body)))
    ((let-keys* ls ((var keyword default) ...) rest-var . body)
     (let ((undef (list 'undef)))
       (let ((var undef) ...)
         (let lp ((pls ls) (acc '()))
           (cond
            ((null? pls)
             (if (eq? var undef) (set! var default)) ...
             (let ((rest-var (reverse! acc)))
               . body))
            (else
             (lp (cddr pls)
                 (case (and (keyword? (car pls))
                            (string->symbol (keyword->string (car pls))))
                   ((keyword) (set! var (cadr pls)) acc) ...
                   (else (cons (cadr pls) (cons (car pls) acc)))))))))))
    ((let-keys* . ?)
     (let-keys-error "malformed let-plist " (let-plist ?)))))

(define-syntax let-keywords*
  (syntax-rules ()
    ((let-keywords* ls specs . body)
     (let-keys* ls () specs . body))))

)

