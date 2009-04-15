;; hato-rfc3028.scm -- sieve: a mail filtering language
;;
;; Copyright (c) 2008 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; http://www.faqs.org/rfcs/rfc3028.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use extras regex srfi-1 srfi-8 srfi-13)

(cond-expand
 ((and chicken compiling)
  (declare (export rfc3028-sieve-read rfc3028-sieve->scheme)))
 (else))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rfc3028-sieve->scheme expr)
  `(call-with-current-continuation
     (lambda (%stop)
       (define mboxes '())
       (define explicit-keep? #f)
       (define (keep . o)
         (if (pair? o)
             (let ((mbox (if (eqv? #\/ (string-ref (car o) 0))
                             (car o)
                             (string-append "~/Mail/" (car o)))))
               (if (not (member mbox mboxes string-ci=?))
                   (set! mboxes (cons mbox mboxes))))
             (set! explicit-keep? #t)))
       (define (ci=? a b)
         (if (and (string? a) (string? b))
             (string-ci=? a b)
             (equal? a b)))
       (define (match? str pat)
         (and (string? str) (string? pat)
              (string-match pat str)))
       (define (contains-ci? a b)
         (and (string? a) (string? b)
              (string-contains-ci a b)))
       (define (over? a b)
         (and (number? a) (number? b) (> a b)))
       (define (under? a b)
         (and (number? a) (number? b) (< a b)))
       (define (same? a b)
         (and (number? a) (number? b) (= a b)))
       (define (localpart addr)
         (cond
          ((and (string? addr) (string-index addr #\@))
           => (lambda (i) (substring addr 0 i)))
          (else addr)))
       (define (domainpart addr)
         (cond
          ((and (string? addr) (string-index addr #\@))
           => (lambda (i) (substring addr (+ i 1) (string-length addr))))
          (else addr)))
       (let ((stop (lambda _ (%stop mboxes))))
         (let ((res ,(sieve->scheme expr)))
           (if (null? mboxes)
               res
               (if (string? res)
                   (cons res mboxes)
                   mboxes)))))))

(define (sieve->scheme expr)
  (cond
   ((symbol? expr)
    (case expr
      ((anyof) 'or)
      ((allof) 'and)
      ((fileinto) 'keep)
      ((true) (lambda () #t))
      ((false) (lambda () #f))
      (else expr)))
   ((pair? expr)
    (case (car expr)
      ((address header)
       (receive (op args) (sieve-extract-op (cdr expr))
         `(or ,@(append-map
                 (lambda (header)
                   (map (lambda (val)
                          `(,op (header ,header) ,val))
                        (if (pair? (cadr args))
                            (cdadr args)
                            (list (cadr args))))) 
                 (if (pair? (car args))
                     (cdar args)
                     (list (car args)))))))
      ((size)
       `(,(sieve-op->scheme (cadr expr)) (size) ,(caddr expr)))
      (else
       (map sieve->scheme expr))))
   (else
    expr)))

(define (sieve-extract-op ls)
  (case (car ls)
    ((all:)
     (sieve-extract-op (cdr ls)))
    ((localpart:)
     (receive (op args) (sieve-extract-op (cdr ls))
       (values op `((localpart ,(car args)) ,@(cdr args)))))
    ((domain:)
     (receive (op args) (sieve-extract-op (cdr ls))
       (values op `((domainpart ,(car args)) ,@(cdr args)))))
    ((matches:)
     (let ((vals (if (and (pair? (caddr ls)) (eq? 'list (caaddr ls)))
                     (cons 'list (map glob->regexp (cdaddr ls)))
                     (if (string? (caddr ls))
                         (glob->regexp (caddr ls))
                         (caddr ls)))))
       (values 'match? (list (cadr ls) vals))))
    (else
     (values (sieve-op->scheme (car ls)) (cdr ls)))))

;; XXXX handle alternate comparators
(define (sieve-op->scheme op)
  (case op
    ((is:) 'ci=?)
    ((contains:) 'contains-ci?)
    ((matches:) 'match?)
    ((over:) 'over?)
    ((under:) 'under?)
    ((equal:) 'same?)
    (else op)))

(define (rfc3028-sieve-read . o)
  (adjust-begins
   (cons 'begin (append-map adjust-exprs (apply rfc3028-sieve-tokenize o)))))

(define (rfc3028-sieve-tokenize . o)
  (parameterize ((case-sensitive #f))
    (let ((in (if (pair? o) (car o) (current-input-port))))
      (let lp ((expr '()) (res '()) (term #f) (stack '()))
        (define (collect)
          (cond
           ((null? expr)
            res)
           ((and (null? (cdr expr))
                 (not (or (symbol? (car expr)) (pair? (car expr)))))
            (cons (car expr) res))
           (else
            (cons (reverse expr) res))))
        (define (save)
          (cons (cons term (cons expr res)) stack))
        (define (stack-term x) (car x))
        (define (stack-expr x) (cadr x))
        (define (stack-res x) (cddr x))
        (let ((c (peek-char in)))
          (if (eof-object? c)
              (if (null? stack)
                  (reverse (collect))
                  (error "incomplete filter"))
              (case c
                ((#\space #\tab #\newline)
                 (read-char in)
                 (lp expr res term stack))
                ((#\#)
                 (do ((c #f (read-char in)))
                     ((or (eof-object? c) (eqv? #\newline c))
                      (lp expr res term stack))))
                ((#\/)
                 (read-char in)
                 (if (not (eqv? #\* (peek-char in)))
                     (lp (cons '/ expr) res term stack)
                     (do ((c1 #f (read-char in))
                          (c2 #f c1))
                         ((or (eof-object? c1)
                              (and (eqv? c1 #\/) (eqv? c2 #\*)))
                          (lp expr res term stack)))))
                ((#\{)
                 (read-char in)
                 (lp '() '(begin) #\{ (save)))
                ((#\[)
                 (read-char in)
                 (lp '() '(list) #\[ (save)))
                ((#\()
                 (read-char in)
                 (lp '() '() #\( (save)))
                ((#\} #\] #\))
                 (let ((c (read-char in)))
                  (cond
                   ((null? stack)
                    (error "closing brace without matching open in filter"))
                   ((not (eqv? c (char-mirror term)))
                    (error "unmatched terminator" c term))
                   (else
                    ;;(warning "stack" stack)
                    (lp (cons (reverse (collect))
                              (stack-expr (car stack)))
                        (stack-res (car stack))
                        (stack-term (car stack))
                        (cdr stack))))))
                ((#\; #\,)
                 (read-char in)
                 (lp '() (collect) term stack))
                ((#\:)
                 ;; prefix keywords
                 (lp (cons (adjust-keyword (read in)) expr) res term stack))
                (else
                 ;; we can use the normal scheme reader for other
                 ;; values, except SI-suffixed numbers which we adjust
                 ;; manually
                 (let* ((val0 (adjust-si-suffix (read in)))
                        (val (if (eq? 'text: val0)
                                 (read-sieve-text in)
                                 val0)))
                   (lp (cons val expr) res term stack)
                   )))))))))

(define (read-sieve-text in)
  ;; skip to end of line
  (do ((c (read-char in) (read-char in)))
      ((or (eof-object? c) (eqv? c #\newline))))
  (let lp ((res '()))
    (let ((line (read-line in)))
      (cond
       ((equal? "" line)
        (if (eof-object? (peek-char in))
            (string-intersperse (reverse res) "\n")
            (lp (cons line res))))
       ((equal? "." line)
        (string-intersperse (reverse (cons "" res)) "\n"))
       ((eqv? #\. (string-ref line 0))
        (lp (cons (substring line 1 (string-length line)) res)))
       (else
        (lp (cons line res)))))))

(define (char-mirror ch)
  (case ch
    ((#\[) #\])  ((#\{) #\})
    ((#\() #\))  ((#\<) #\>)
    (else ch)))

(define (adjust-keyword val)
  (if (not (symbol? val))
      val
      (let ((str (symbol->string val)))
        (if (not (eqv? #\: (string-ref str 0)))
            val
            (string->keyword (substring str 1 (string-length str)))))))

(define (adjust-si-suffix val)
  (if (not (symbol? val))
      val
      (let* ((str (symbol->string val))
             (len (string-length str))
             (c (char-upcase (string-ref str (- len 1)))))
        (if (not (memv c '(#\K #\M #\G)))
            val
            (let ((n (string->number
                      (substring str 0 (- len 1)))))
              (if (not n)
                  val
                  (* n (case c
                         ((#\G) (* 1024 1024 1024))
                         ((#\M) (* 1024 1024))
                         (else 1024)))))))))

(define (adjust-exprs x)
  (if (not (pair? x))
      x
      (if (not (symbol? (car x)))
          (cons (car x) (adjust-exprs (cdr x)))
          (case (car x)
            ((begin)
             (cons 'begin (append-map adjust-exprs (cdr x))))
            ((if elsif)
             (let ((res (adjust-exprs (cdr x))))
               (match res
                 ((check pass ('elsif . branch2) . rest)
                  `((,(car x) ,check ,(adjust-exprs pass) (if ,@branch2))
                    ,@rest))
                 ((check pass ('else fail) . rest)
                  `((,(car x) ,check ,(adjust-exprs pass) ,fail) ,@rest))
                 ((check pass . rest)
                  `((,(car x) ,check ,(adjust-exprs pass)) ,@rest))
                 (else
                  (error "invalid IF clause" x)))))
            ((anyof allof)
             (let ((res (adjust-exprs (cdr x))))
               (cons (cons (car x) (append-map adjust-exprs (car res)))
                     (cdr res))))
            ((not else)
             (let ((res (adjust-exprs (cdr x))))
               (cons (list (car x) (car res)) (cdr res))))
            (else
             (receive (args rest)
                 (break (lambda (x)
                          (or (and (symbol? x) (not (keyword? x)))
                              (and (pair? x) (eq? 'begin (car x)))))
                        (cdr x))
               `((,(car x) ,@args)
                 ,@(adjust-exprs rest))))))))

(define (adjust-begins x)
  (if (pair? x)
      (if (and (eq? 'begin (car x)) (pair? (cdr x)) (null? (cddr x)))
          (adjust-begins (cadr x))
          (map adjust-begins x))
      x))
