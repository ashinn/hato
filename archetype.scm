
(require-library matchable srfi-1 srfi-13)

(module archetype

(archetype-parse archetype-tokenize archetype-expand archetype-file
 archetype->html)

(import scheme chicken extras data-structures matchable ports srfi-1 srfi-13)

(define (make-archetype-env)
  (list (cons 'define archetype-define)
        (cons 'lambda archetype-lambda)))

(define (env-ref env sym . o)
  (cond ((assq sym env) => cdr)
        ((pair? o) (car o))
        (else #f)))

(define (env-set! env sym val)
  (cond ((assq sym env) => (lambda (pair) (set-cdr! pair val)))
        (else (set-cdr! env (cons (cons sym val) (cdr env))))))

(define (env-extend env vars vals)
  (append (map cons vars vals) env))

(define (map* proc ls)
  (cond ((null? ls) '())
        ((pair? ls) (cons (proc (car ls)) (map* proc (cdr ls))))
        (else (proc ls))))

(define (append-map* proc ls)
  (cond ((null? ls) '())
        ((pair? ls) (append (proc (car ls)) (append-map* proc (cdr ls))))
        (else (proc ls))))

(define (body-argument ls)
  (cond
   ((null? ls)
    "")
   ((member (car ls) '("" " "))
    (body-argument (cdr ls)))
   ((null? (cdr ls))
    (if (string? (car ls)) (string-trim (car ls)) (car ls)))
   (else
    (cons (if (string? (car ls)) (string-trim (car ls)) (car ls)) (cdr ls)))))

(define (archetype-define expr env)
  (if (or (null? (cdr expr)) (null? (cddr expr)))
      (error "archetype: not enough arguments to define" expr))
  (cond
   ((pair? (cadr expr))
    (let ((params (append-map*
                   (lambda (x)
                     (if (string? x)
                         (map string->symbol (string-split x))
                         (list x)))
                   (cadr expr))))
      (env-set! env
                (car params)
                (archetype-lambda `(lambda ,(cdr params) ,@(cddr expr)) env))))
   (else
    (env-set! env
              (cadr expr)
              (archetype-expand (body-argument (cddr expr)) env))))
  '())

(define (archetype-lambda expr env)
  (if (or (null? (cdr expr)) (null? (cddr expr)))
      (cond
       ((and (string? (cadr expr))
             (not (equal? "" (cadr expr)))
             (eqv? #\{ (string-ref (cadr expr) 0))
             (string-index (cadr expr) #\}))
        => (lambda (i)
             (archetype-lambda
              `(lambda ,(map* string->symbol
                         (string-split (substring (cadr expr) 1 i)))
                 ,(substring (cadr expr) (+ i 1)))
              env)))
       (else
        (error "archetype: not enough arguments to lambda" expr)))
      (let ((vars (map* (lambda (x) (if (string? x) (string->symbol x) x))
                        (cadr expr)))
            (body (body-argument (cddr expr))))
        (lambda (expr2 env2)
          (let ((env3 (env-extend env2 vars (cdr expr2))))
            (archetype-expand body env3))))))

(define (archetype-file file . o)
  (let ((env (if (pair? o) (car o) (make-archetype-env))))
    (archetype-fix-document
     (call-with-input-file file (lambda (in) (archetype-parse in env))))))

(define (archetype-parse src . o)
  (let ((env (if (pair? o) (car o) (make-archetype-env)))
        (in (if (string? src) (open-input-string src) src)))
    (archetype-fix-begins
     (archetype-expand (archetype-tokenize in) env))))

(define (archetype-tokenize in)
  (define (collect str res)
      (if (pair? str) (cons (reverse-list->string str) res) res))
  (define (skip-comment)
    (let ((c (read-char in)))
      (if (not (or (eof-object? c) (eqv? c #\newline)))
          (skip-comment))))
  (define (read-identifier str)
    (let ((c (peek-char in)))
      (cond
        ((or (eof-object? c) (char-whitespace? c) (memv c '(#\\ #\{ #\} #\%)))
         (string->symbol (reverse-list->string str)))
        (else
         (read-char in)
         (read-identifier (cons c str))))))
  (define (tok str res depth)
    (let ((c (read-char in)))
      (cond
        ((eof-object? c)
         (reverse (collect str res)))
        ((eqv? c #\\)
         (let ((c (read-char in)))
           (cond
             ((eof-object? c)
              (error "incomplete trailing slash"))
             ((eqv? c #\{)
              (tok '()
                   (cons (tok '() '() (+ depth 1)) (collect str res))
                   depth))
             ((not (or (char-whitespace? c) (memv c '(#\\ #\{ #\} #\%))))
              (let ((id (read-identifier (list c)))
                    (res (collect str res)))
                (cond
                  ((eqv? #\{ (peek-char in))
                   (read-char in)
                   (tok '() (cons (tok '() (list id) (+ depth 1)) res) depth))
                  (else
                   (tok '() (cons id res) depth)))))
             (else
              (tok (cons c str) res depth)))))
        ((eqv? c #\{)
         (cond
           ((eqv? #\\ (peek-char in))
            (read-char in)
            (let ((id (read-identifier '()))
                  (res (collect str res)))
              (if (char-whitespace? (peek-char in))
                  (read-char in))
              (tok '() (cons (tok '() (list id) (+ depth 1)) res) depth)))
           (else
            (tok (cons c str) res depth))))
        ((eqv? c #\})
         (if (zero? depth)
             (tok (cons #\} str) res 0) ; maybe warn?
             (reverse (collect str res))))
        ((eqv? c #\%) ; maybe make this \%?
         (skip-comment)
         (tok str res depth))
        (else
         (tok (cons c str) res depth)))))
  ;; begin
  (tok '() '() 0))

(define (archetype-expand expr env)
  (let expand ((x expr))
    (cond
      ((pair? x)
       (cond
         ((not (proper-list? x))
          (error "not a proper list: " x))
         ((symbol? (car x))
          (let ((op (env-ref env (car x))))
            (if op
                (cond
                  ((procedure? op)
                   (expand (op x env)))
                  (else
                   (map-in-order expand x)))
                (map-in-order expand x))))
         (else
          (map-in-order expand x))))
      ((symbol? x)
       (let ((val (env-ref env x)))
         (if val
             (expand val)
             x)))
      (else
       x))))

;; (define (archetype-fix-begins x)
;;   (if (not (pair? x))
;;       x
;;       (let lp ((ls x)
;;                (cur '())
;;                (tag '())
;;                (stack '()))
;;         (cond
;;           ((null? ls)
;;            (let ((res (reverse cur)))
;;              (if (pair? stack)
;;                  (lp (cdar stack) '() (caar stack) (cdr stack))
;;                  res)))
;;           (else
;;            (match (car ls)
;;              (('begin tag2)
;;               (cond
;;                 ((equal? tag tag2)
;;                  (lp (cdr ls) (reverse cur) '() tag2))
;;                 (tag
;;                  (lp (cdr ls) res (cons (car ls) res) tag))
;;                 (else
;;                  (lp (cdr ls) (reverse cur) '() tag2))))
;;              (('end tag2) ;; XXXX verify/adjust tag
;;               (lp (cdr ls) (reverse cur) '() #f))
;;              (else
;;               (if tag
;;                   (lp (cdr ls) res (cons (car ls) cur) tag)
;;                   (lp (cdr ls) (cons (car ls) res) '() #f)))))))))

(define (archetype-fix-begins x) x)

(define (archetype-fix-document expr)
  (or (let check ((x expr))
        (if (pair? x)
            (if (eq? 'document (car x))
                x
                (any check x))))
      (list 'document expr)))

(define (archetype->html expr)
  (sxml->html expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple conversions

(define (html-display-escaped-attr str)
  (let ((start 0)
        (end (string-length str)))
    (let lp ((from start) (to start))
      (if (>= to end)
        (display (substring str from to))
        (let ((c (string-ref str to)))
          (cond
            ((eq? c #\<)
             (display (substring str from to))
             (display "&lt;")
             (lp (fx+ to 1) (fx+ to 1)))
            ((eq? c #\&)
             (display (substring str from to))
             (display "&amp;")
             (lp (fx+ to 1) (fx+ to 1)))
            ((eq? c #\")
             (display (substring str from to))
             (display "&quot;")
             (lp (fx+ to 1) (fx+ to 1)))
            (else
             (lp from (fx+ to 1)))))))))

(define (html-escape-attr str)
  (with-output-to-string
    (lambda () (html-display-escaped-attr str))))

(define (html-attr->string attr)
  (if (cdr attr)
      (string-append (symbol->string (car attr))
                     "=\"" (html-escape-attr (cdr attr)) "\"")
      (symbol->string (car attr))))

(define (html-tag->string tag attrs)
  (let lp ((ls attrs) (res (list (symbol->string tag) "<")))
    (if (null? ls)
      (apply string-append (reverse (cons ">" res)))
      (lp (cdr ls) (cons (html-attr->string (car ls)) (cons " " res))))))

(define (html-display-escaped-string str)
  (let ((start 0)
        (end (string-length str)))
    (let lp ((from start) (to start))
      (if (>= to end)
        (display (substring str from to))
        (let ((c (string-ref str to)))
          (cond
            ((eq? c #\<)
             (display (substring str from to))
             (display "&lt;")
             (lp (fx+ to 1) (fx+ to 1)))
            ((eq? c #\&)
             (display (substring str from to))
             (display "&amp;")
             (lp (fx+ to 1) (fx+ to 1)))
            (else
             (lp from (fx+ to 1)))))))))

(define (html-escape str)
  (with-output-to-string
    (lambda () (html-display-escaped-string str))))

(define (sxml-display-as-html sxml . o)
  (cond
    ((pair? sxml)
     (let ((tag (car sxml)))
       (if (symbol? tag)
         (let ((rest (cdr sxml)))
           (cond
             ((and (pair? rest)
                   (pair? (car rest))
                   (eq? '@ (caar rest)))
              (display (html-tag->string tag (cdar rest)))
              (for-each sxml-display-as-html (cdr rest))
              (display "</") (display tag) (display ">"))
             (else
              (display (html-tag->string tag '()))
              (for-each sxml-display-as-html rest)
              (display "</") (display tag) (display ">"))))
         (for-each sxml-display-as-html sxml))))
    ((null? sxml))
    (else (html-display-escaped-string sxml))))

(define (sxml->html sxml . o)
  (with-output-to-string
    (lambda () (apply sxml-display-as-html sxml o))))

)
