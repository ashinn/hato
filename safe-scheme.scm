
(require-library srfi-1)

(module safe-scheme
  (not boolean? eq? eqv? equal? pair?
   cons car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr
   cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar
   cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr set-car! set-cdr!
   null? list? list length list-tail list-ref append reverse memq memv
   member assq assv assoc symbol? symbol->string string->symbol number?
   integer? exact? real? complex? inexact? rational? zero? odd? even?
   positive? negative?  max min + - * / = > < >= <= quotient remainder
   modulo gcd lcm abs floor ceiling truncate round exact->inexact
   inexact->exact exp log expt sqrt sin cos tan asin acos atan
   number->string string->number char? char=? char>? char<? char>=?
   char<=? char-ci=? char-ci<? char-ci>?  char-ci>=? char-ci<=?
   char-alphabetic? char-whitespace? char-numeric? char-upper-case?
   char-lower-case? char-upcase char-downcase char->integer integer->char
   string? string=?  string>? string<? string>=? string<=? string-ci=?
   string-ci<? string-ci>? string-ci>=? string-ci<=?  make-string
   string-length string-ref string-set! string-append string-copy
   string->list list->string substring string-fill! vector? make-vector
   vector-ref vector-set! string vector vector-length vector->list
   list->vector vector-fill! procedure? map for-each apply force
   call-with-current-continuation input-port? output-port?
   current-input-port current-output-port
   close-input-port close-output-port read eof-object? read-char
   peek-char write display write-char newline
   dynamic-wind values call-with-values eval
   char-ready? imag-part real-part magnitude numerator denominator
;;    define set! lambda if begin quote quasiquote unquote unquote-splicing
;;    let let* letrec and or cond case do
;;    define-syntax let-syntax letrec-syntax syntax-rules
;;    er-macro-transformer
   cond do or and
   )

(import (rename (except scheme open-input-file open-output-file)
;;                 (define-syntax %define-syntax)
;;                 (let-syntax %let-syntax)
;;                 (letrec-syntax %letrec-syntax)
                (eval %eval))
        (only chicken error gensym)
        (only data-structures substring-index)
        (only srfi-1 every))

;; currently just checks for # in symbols, which can make explicit
;; references to variables in external namespaces
(define (safe-expression? x)
  (cond
   ((symbol? x)
    (not (substring-index "#" (symbol->string x))))
   ((pair? x)
    (and (safe-expression? (car x))
         (safe-expression? (cdr x))))
   ((vector? x)
    (let lp ((i (- (vector-length x) 1)))
      (if (zero? i)
          (safe-expression? (vector-ref x 0))
          (and (safe-expression? (vector-ref x i))
               (lp (- i 1))))))
   ((or (null? x) (number? x) (char? x) (string? x) (eq? x (if #f #f)))
    #t)
   (else
    #f)))

;; (define (safe-wrap-syntax macro)
;;   (er-macro-transformer
;;    (lambda (e r c)
;;      (let ((res (macro e r c)))
;;        (if (safe-expression? res)
;;            res
;;            (error "macro expanded into unsafe expression" e res))))))

;; (%define-syntax define-syntax
;;   (er-macro-transformer
;;    (lambda (e r c)
;;      (if (not (= 3 (length e)))
;;          (error "invalid define-syntax" e)
;;          (let ((macro (gensym 'macro)))
;;            `(,(r '%define-syntax) ,(cadr e)
;;              (,(r 'safe-wrap-syntax) ,(caddr e))))))))

;; (%define-syntax let-syntax
;;   (er-macro-transformer
;;    (lambda (e r c)
;;      (if (or (< (length e) 3)
;;              (not (list? (cadr e)))
;;              (every (lambda (x) (and (list? x) (= 2 (length x)))) (cadr e)))
;;          (error "invalid let-syntax" e)
;;          `(,(r '%let-syntax) ,(map (lambda (x) `(,(r 'safe-wrap-syntax) ,x))
;;                                    (cadr e))
;;            ,@(cddr e))))))

;; (%define-syntax letrec-syntax
;;   (er-macro-transformer
;;    (lambda (e r c)
;;      (if (or (< (length e) 3)
;;              (not (list? (cadr e)))
;;              (every (lambda (x) (and (list? x) (= 2 (length x)))) (cadr e)))
;;          (error "invalid letrec-syntax" e)
;;          `(,(r '%letrec-syntax) ,(map (lambda (x) `(,(r 'safe-wrap-syntax) ,x))
;;                                       (cadr e))
;;            ,@(cddr e))))))

(define (eval expr . o)
  (if (safe-expression? expr)
      (%eval `(module ,(gensym 'safe) (import safe-scheme) expr))
      (error "unsafe expression passed to eval" expr)))

)

