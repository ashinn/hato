;;;; let-args.scm -- friendly syntax for command-line arguments
;;
;; Copyright (c) 2003-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(use srfi-1)

(module let-args (let-args)

(import scheme chicken data-structures)
(import-for-syntax srfi-1)

(define-for-syntax (split-improper-list ls)
  (let loop ((l ls) (res '()))
    (if (pair? l)
      (loop (cdr l) (cons (car l) res))
      (values (reverse res) l))))

(define-for-syntax (type-coercer+next type)
  (cond
    ((or (not type) (equal? type ""))
     (values #f #f))
    ((string? type)
     (cond
      ((or (= 1 (string-length type))
           (and (= 2 (string-length type)) (eqv? #\* (string-ref type 1))))
       (type-coercer+next (string-ref type 0)))
      (else
       (let lp ((ls (string->list type))
                (res #f))
         (if (null? ls)
             (values (car res) (cdr res))
             (receive (c2 n2) (type-coercer+next (car ls))
               (lp (cdr ls)
                   (if res
                       (cons `(lambda (ls)
                                (cons (,c2 (car ls)) (,(car res) (cdr ls))))
                             `(lambda (ls) (,n2 (,(cdr res) ls)))
                             (cons `(lambda (ls) (cons (,c2 (cadr ls)) '()))
                                   n2))))))))))
    (else
     (case type
       ((#\s #\*)
        (values 'cadr 'cddr))
       ((#\n)
        (values '(lambda (ls) (string->number (cadr ls))) 'cddr))
       ((#\i)
        (values '(lambda (ls) (inexact->exact (string->number (cadr ls)))) 'cddr))
       ((#\f)
        (values '(lambda (ls) (exact->inexact (string->number (cadr ls)))) 'cddr))
       ((#\e)
        (values '(lambda (ls) (with-input-from-string (car ls) read)) 'cddr))
       ((#\y)
        (values '(lambda (ls) (string->symbol (cadr ls))) 'cddr))
       (else (error "unknown type: " type))))))

;; normalize: var keywords default action doc
(define-for-syntax (extract-spec x ls kont)
  (cond
    ((symbol? x)
     (extract-spec (list x) ls kont))
    ((and (list? x) (<= 1 (length x) 6) (symbol? (car x)))
     (let ((var (car x)))
       (if (eq? var 'else)
         (vector var #f #f (cons 'lambda (cdr x)))
         (receive (info act) (span (lambda (y) (not (eq? y '=>))) (cdr x))
           (let* ((key-string (or (and (pair? info) (car info))
                                  (symbol->string var)))
                  (keys+type (string-split key-string "="))
                  (keys (map string->symbol (string-split (car keys+type) "|")))
                  (type (and (pair? (cdr keys+type)) (cadr keys+type)))
                  (multi?
                   (and type
                        (>= (string-length type) 1)
                        (eqv? #\* (string-ref type
                                              (- (string-length type) 1)))))
                  (doc (if (pair? act)
                         (and (pair? (cdr act)) (pair? (cddr act)) (caddr act))
                         (and (>= (length info) 3) (caddr info)))))
             (receive (coercer next) (type-coercer+next type)
               (vector
                var
                keys
                (if (and (pair? info) (pair? (cdr info)))
                    (cadr info)
                    (if multi? ''() #f))
                (if (pair? act)
                  `((begin
                      (,(cadr act) ,(if coercer (list coercer ls) #t))
                      (,kont ,(if next (list next ls) `(cdr ,ls)))))
                  (let ((act (if coercer (list coercer ls) #t)))
                    `((begin
                       (set! ,var ,(if multi? `(cons ,act ,var) act))
                       (,kont ,(if next (list next ls) `(cdr ,ls)))))))
                doc)))))))
    (else (error "bad binding in let-args:" x))))

(define-for-syntax (spec-var x) (vector-ref x 0))
(define-for-syntax (spec-keywords x) (vector-ref x 1))
(define-for-syntax (spec-default x) (vector-ref x 2))
(define-for-syntax (spec-action x) (vector-ref x 3))
(define-for-syntax (spec-doc x) (vector-ref x 4))

(define-for-syntax (spec-eval-default? x)
  (or (symbol? (spec-default x))
      (and (pair? (spec-default x))
           (not (eq? 'quote (car (spec-default x)))))))
(define-for-syntax (spec-literal-default? x)
  (not (spec-eval-default? x)))

(define-for-syntax (build-let-args init-ls vars rest-var body)
  (let ((rest-var (or rest-var (gensym 'rest)))
        (loop (gensym 'loop))
        (ls (gensym 'ls))
        (str (gensym 'str))
        (len (gensym 'len))
        (i (gensym 'i))
        (finish (gensym 'finish)))
    (let ((specs (map (lambda (x) (extract-spec x ls loop)) vars)))
      (receive (specs else-clause)
          (span (lambda (x) (not (eq? 'else (spec-var x)))) specs)
        (if (and (pair? else-clause) (pair? (cdr else-clause)))
            (error "clauses appear after else"))
        `(let* (,@(map (lambda (x)
                         `(,(spec-var x)
                           ,(if (spec-literal-default? x)
                                (spec-default x)
                                (list 'delay (spec-default x)))))
                       specs))
           (let ((,finish
                  (lambda (,rest-var)
                    ,@(map (lambda (x)
                             `(set! ,(spec-var x) (force ,(spec-var x))))
                           (filter (lambda (x)
                                     (and (spec-var x)
                                          (spec-eval-default? x)))
                                   specs))
                    ,@body)))
             (let ,loop ((,ls ,init-ls))
                  (if (null? ,ls)
                      (,finish ,ls)
                      (let* ((,str (car ,ls))
                             (,len (string-length ,str)))
                        (if (or (zero? ,len)
                                (not (eq? #\- (string-ref ,str 0)))
                                (= ,len 1)
                                (and (= ,len 2) (eq? #\- (string-ref ,str 1))))
                            (,finish ,ls)
                            (begin
                              (let ,loop ((,i 2))
                                   (unless (= ,i ,len)
                                     (if (eq? #\= (string-ref ,str ,i))
                                         (begin
                                           (set! ,ls
                                                 (cons (car ,ls)
                                                       (cons (substring ,str (+ ,i 1))
                                                             (cdr ,ls))))
                                           (set! ,str (substring ,str 0 ,i)))
                                         (,loop (+ ,i 1)))))
                              (case (string->symbol
                                     (substring
                                      ,str
                                      (if (eq? #\- (string-ref ,str 1)) 2 1)))
                                ,@(map
                                   (lambda (x)
                                     (cons (spec-keywords x) (spec-action x)))
                                   specs)
                                (else
                                 ,(if (pair? else-clause)
                                      `(,(spec-action (car else-clause))
                                        ,str (cdr ,ls) ,loop)
                                      `(,finish (cdr ,ls))))))))))))))))

(define-syntax let-args
  (er-macro-transformer
   (lambda (expr rename compare)
     (apply
      (lambda (ls vars . body)
        (if (proper-list? vars)
            (build-let-args ls vars #f body)
            (receive (vars rest-var) (split-improper-list vars)
              (build-let-args ls vars rest-var body))))
      (cdr expr)))))

)
