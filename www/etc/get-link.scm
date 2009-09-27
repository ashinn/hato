
(use ports srfi-1 srfi-13 matchable html-parser)

(define symbols
  '((define (syntax (name value) undefined) "define a new variable")
    (set! (syntax (name value) undefined) "set the value of a variable")
    (let (syntax (((var val) ...) body ...)) "bind new local variables in parallel")
    (let* (syntax (((var val) ...) body ...)) "bind new local variables sequentially")
    (letrec (syntax (((var val) ...) body ...)) "bind new local variables recursively")
    (lambda (syntax (params body ...)) "procedure syntax")
    (if (syntax (cond then else)) "conditional evaluation")
    (cond (syntax (clause ...)) "try each clause until one succeeds")
    (case (syntax (expr clause ...)) "look for EXPR among literal lists")
    (delay (syntax (expr)) "create a promise to evaluate EXPR")
    (and (syntax (expr ...)) "evaluate EXPRs while true, return last")
    (or (syntax (expr ...)) "return the first true EXPR")
    (begin (syntax (expr ...)) "evaluate each EXPR in turn and return the last")
    (do (syntax (vars finish body ...)) "simple iterator")
    (quote (syntax (expr)) "represent EXPR literally without evaluating it")
    (quasiquote (syntax (expr)) "quote literals allowing escapes")
    (unquote (syntax (expr)) "escape an expression inside quasiquote")
    (unquote-splicing (syntax (expr)) "escape and splice a list expression inside quasiquote")
    (define-syntax (syntax (name body ...) undefined) "create a macro")
    (let-syntax (syntax (syntaxes body ...)) "a local macro")
    (letrec-syntax (syntax (syntaxes body ...)) "a local macro")
    (syntax-rules (syntax (literals clauses ...) undefined) "simple macro language")
    (eqv? (lambda (obj1 obj2) bool) "returns #t if OBJ1 and OBJ2 are the same object")
    (eq? (lambda (obj1 obj2) bool) "finer grained version of EQV?")
    (equal? (lambda (obj1 obj2) bool) "recursive equivalence")
    (not (lambda (obj) bool) "returns #t iff OBJ is false")
    (boolean? (lambda (obj) bool) "returns #t iff OBJ is #t or #f")
    (number? (lambda (obj) bool) "returns #t iff OBJ is a number")
    (complex? (lambda (obj) bool) "returns #t iff OBJ is a complex number")
    (real? (lambda (obj) bool) "returns #t iff OBJ is a real number")
    (rational? (lambda (obj) bool) "returns #t iff OBJ is a rational number")
    (integer? (lambda (obj) bool) "returns #t iff OBJ is an integer")
    (exact? (lambda (z) bool) "returns #t iff Z is exact")
    (inexact? (lambda (z) bool) "returns #t iff Z is inexact")
    (= (lambda (z1 z2 ...) bool) "returns #t iff the arguments are all equal")
    (< (lambda (x1 x2 ...) bool) "returns #t iff the arguments are monotonically increasing")
    (> (lambda (x1 x2 ...) bool) "returns #t iff the arguments are monotonically decreasing")
    (<= (lambda (x1 x2 ...) bool) "returns #t iff the arguments are monotonically nondecreasing")
    (>= (lambda (x1 x2 ...) bool) "returns #t iff the arguments are monotonically nonincreasing")
    (zero? (lambda (z) bool))
    (positive? (lambda (x1) bool))
    (negative? (lambda (x1) bool))
    (odd? (lambda (n) bool))
    (even? (lambda (n) bool))
    (max (lambda (x1 x2 ...) x3) "returns the maximum of the arguments")
    (min (lambda (x1 x2 ...) x3) "returns the minimum of the arguments")
    (+ (lambda (z1 ...) z))
    (* (lambda (z1 ...) z))
    (- (lambda (z1 ...) z))
    (/ (lambda (z1 ...) z))
    (abs (lambda (x1) x2) "returns the absolute value of X")
    (quotient (lambda (n1 n2) n) "integer division")
    (remainder (lambda (n1 n2) n) "same sign as N1")
    (modulo (lambda (n1 n2) n) "same sign as N2")
    (gcd (lambda (n1 ...) n) "greatest common divisor")
    (lcm (lambda (n2 ...) n) "least common multiple")
    (numerator (lambda (rational) n))
    (denominator (lambda (rational) n))
    (floor (lambda (x1) n) "largest integer not larger than X")
    (ceiling (lambda (x1) n) "smallest integer not smaller than X")
    (truncate (lambda (x1) n) "drop fractional part")
    (round (lambda (x1) n) "round to even (banker's rounding)")
    (rationalize (lambda (x1 y) n) "rational number differing from X by at most Y")
    (exp (lambda (z) z) "e^Z")
    (log (lambda (z) z) "natural logarithm of Z")
    (sin (lambda (z) z) "sine function")
    (cos (lambda (z) z) "cosine function")
    (tan (lambda (z) z) "tangent function")
    (asin (lambda (z) z) "arcsine function")
    (acos (lambda (z) z) "arccosine function")
    (atan (lambda (z) z) "arctangent function")
    (sqrt (lambda (z) z) "principal square root of Z")
    (expt (lambda (z1 z2) z) "returns Z1 raised to the Z2 power")
    (make-rectangular (lambda (x1 x2) z) "create a complex number")
    (make-polar (lambda (x1 x2) z) "create a complex number")
    (real-part (lambda (z) x1))
    (imag-part (lambda (z) x1))
    (magnitude (lambda (z) x1))
    (angle (lambda (z) x1))
    (exact->inexact (lambda (z) z))
    (inexact->exact (lambda (z) z))
    (number->string (lambda (z :optional radix) str))
    (string->number (lambda (str :optional radix) z))
    (pair? (lambda (obj) bool) "returns #t iff OBJ is a pair")
    (cons (lambda (obj1 obj2) pair) "create a newly allocated pair")
    (car (lambda (pair) obj))
    (cdr (lambda (pair) obj))
    (set-car! (lambda (pair obj) undefined))
    (set-cdr! (lambda (pair obj) undefined))
    (caar (lambda (pair) obj))
    (cadr (lambda (pair) obj))
    (cdar (lambda (pair) obj))
    (cddr (lambda (pair) obj))
    (caaar (lambda (pair) obj))
    (caadr (lambda (pair) obj))
    (cadar (lambda (pair) obj))
    (caddr (lambda (pair) obj))
    (cdaar (lambda (pair) obj))
    (cdadr (lambda (pair) obj))
    (cddar (lambda (pair) obj))
    (cdddr (lambda (pair) obj))
    (caaaar (lambda (pair) obj))
    (caaadr (lambda (pair) obj))
    (caadar (lambda (pair) obj))
    (caaddr (lambda (pair) obj))
    (cadaar (lambda (pair) obj))
    (cadadr (lambda (pair) obj))
    (caddar (lambda (pair) obj))
    (cadddr (lambda (pair) obj))
    (cdaaar (lambda (pair) obj))
    (cdaadr (lambda (pair) obj))
    (cdadar (lambda (pair) obj))
    (cdaddr (lambda (pair) obj))
    (cddaar (lambda (pair) obj))
    (cddadr (lambda (pair) obj))
    (cdddar (lambda (pair) obj))
    (cddddr (lambda (pair) obj))
    (null? (lambda (obj) bool) "returns #t iff OBJ is the empty list")
    (list? (lambda (obj) bool) "returns #t iff OBJ is a proper list")
    (list (lambda (obj ...) list) "returns a newly allocated list")
    (length (lambda (list) n))
    (append (lambda (list ...) list) "concatenates the list arguments")
    (reverse (lambda (list) list))
    (list-tail (lambda (list k) list) "returns the Kth cdr of LIST")
    (list-ref (lambda (list k) obj) "returns the Kth element of LIST")
    (memq (lambda (obj list)) "the sublist of LIST whose car is eq? to OBJ")
    (memv (lambda (obj list)) "the sublist of LIST whose car is eqv? to OBJ")
    (member (lambda (obj list)) "the sublist of LIST whose car is equal? to OBJ")
    (assq (lambda (obj list)) "the element of LIST whose car is eq? to OBJ")
    (assv (lambda (obj list)) "the element of LIST whose car is eqv? to OBJ")
    (assoc (lambda (obj list)) "the element of LIST whose car is equal? to OBJ")
    (symbol? (lambda (obj) bool) "returns #t iff OBJ is a symbol")
    (symbol->string (lambda (symbol) str))
    (string->symbol (lambda (str) symbol))
    (char? (lambda (obj) bool) "returns #t iff OBJ is a character")
    (char=? (lambda (ch1 ch2) bool))
    (char<? (lambda (ch1 ch2) bool))
    (char>? (lambda (ch1 ch2) bool))
    (char<=? (lambda (ch1 ch2) bool))
    (char>=? (lambda (ch1 ch2) bool))
    (char-ci=? (lambda (ch1 ch2) bool))
    (char-ci<? (lambda (ch1 ch2) bool))
    (char-ci>? (lambda (ch1 ch2) bool))
    (char-ci<=? (lambda (ch1 ch2) bool))
    (char-ci>=? (lambda (ch1 ch2) bool))
    (char-alphabetic? (lambda (ch) bool))
    (char-numeric? (lambda (ch) bool))
    (char-whitespace? (lambda (ch) bool))
    (char-upper-case? (lambda (ch) bool))
    (char-lower-case? (lambda (ch) bool))
    (char->integer (lambda (ch) int))
    (integer->char (lambda (int) ch))
    (char-upcase (lambda (ch) ch))
    (char-downcase (lambda (ch) ch))
    (string? (lambda (obj) bool) "returns #t iff OBJ is a string")
    (make-string (lambda (k :optional ch) str) "a new string of length k")
    (string (lambda (ch ...) str) "a new string made of the char arguments")
    (string-length (lambda (str) n) "the number of characters in STR")
    (string-ref (lambda (str i) ch) "the Ith character of STR")
    (string-set! (lambda (str i ch) undefined) "set the Ith character of STR to CH")
    (string=? (lambda (str1 str2) bool))
    (string-ci=? (lambda (str1 str2) bool))
    (string<? (lambda (str1 str2) bool))
    (string>? (lambda (str1 str2) bool))
    (string<=? (lambda (str1 str2) bool))
    (string>=? (lambda (str1 str2) bool))
    (string-ci<? (lambda (str1 str2) bool))
    (string-ci>? (lambda (str1 str2) bool))
    (string-ci<=? (lambda (str1 str2) bool))
    (string-ci>=? (lambda (str1 str2) bool))
    (substring (lambda (str start end) str))
    (string-append (lambda (str ...) str) "concatenate the string arguments")
    (string->list (lambda (str) list))
    (list->string (lambda (list) str))
    (string-copy (lambda (str) str))
    (string-fill! (lambda (str ch) undefined) "set every char in STR to CH")
    (vector? (lambda (obj) bool) "returns #t iff OBJ is a vector")
    (make-vector (lambda (len :optional fill) vec) "a new vector of K elements")
    (vector (lambda (obj ...) vec))
    (vector-length (lambda (vec) n) "the number of elements in VEC")
    (vector-ref (lambda (vec i) obj) "the Ith element of VEC")
    (vector-set! (lambda (vec i obj) undefined) "set the Ith element of VEC to OBJ")
    (vector->list (lambda (vec) list))
    (list->vector (lambda (list) vec))
    (vector-fill! (lambda (vec obj) undefined) "set every element in VEC to OBJ")
    (procedure? (lambda (obj) bool) "returns #t iff OBJ is a procedure")
    (apply (lambda ((lambda obj a) obj ...) a) "procedure application")
    (map (lambda ((lambda (obj1 . obj2) a) list ...) (list a)) "a new list of PROC applied to every element of LIST")
    (for-each (lambda ((lambda obj a) obj ...) undefined) "apply PROC to each element of LIST in order")
    (force (lambda (promise) obj) "force the delayed value of PROMISE")
    (call-with-current-continuation (lambda (proc) obj) "obtain the continuation (next step) of the current computation as a first-class value")
    (values (lambda (obj ...)) "send multiple values to the calling continuation")
    (call-with-values (lambda (producer consumer) obj))
    (dynamic-wind (lambda (before-thunk thunk after-thunk) obj))
    (scheme-report-environment (lambda (int) env) "INT should be 5")
    (null-environment (lambda (int) env) "INT should be 5")
    (call-with-input-file (lambda (path proc) input-port))
    (call-with-output-file (lambda (path proc) output-port))
    (input-port? (lambda (obj) bool) "returns #t iff OBJ is an input port")
    (output-port? (lambda (obj) bool) "returns #t iff OBJ is an output port")
    (current-input-port (lambda () input-port) "the default input for read procedures")
    (current-output-port (lambda () output-port) "the default output for write procedures")
    (with-input-from-file (lambda (path thunk) obj))
    (with-output-to-file (lambda (path thunk) obj))
    (open-input-file (lambda (path) input-port))
    (open-output-file (lambda (path) output-port))
    (close-input-port (lambda (input-port)))
    (close-output-port (lambda (output-port)))
    (read (lambda (:optional input-port) obj) "read a datum")
    (read-char (lambda (:optional input-port) ch) "read a single character")
    (peek-char (lambda (:optional input-port) ch))
    (eof-object? (lambda (obj) bool) "returns #t iff OBJ is the end-of-file object")
    (char-ready? (lambda (:optional input-port) bool))
    (write (lambda (object :optional output-port) undefined) "write a datum")
    (display (lambda (object :optional output-port) undefined) "display")
    (newline (lambda (:optional output-port) undefined) "send a linefeed")
    (write-char (lambda (char :optional output-port) undefined) "write a single character")
    (load (lambda (filename) undefined) "evaluate expressions from a file")
    (eval (lambda (expr env)))))

(define index (call-with-input-file "r5rs-Z-H-15.html" html->sxml))

(define (gen-doc sym type . o)
  (with-output-to-string
    (lambda ()
      (cond
       ((and (pair? type) (eq? 'lambda (car type)))
        (display "(<span class=\"keyword\">")
        (write sym)
        (display "</span>")
        (let lp ((ls (cadr type)))
          (cond ((null? ls))
                ((eq? ':optional (car ls))
                 (display " [")
                 (write (cadr ls))
                 (for-each
                  (lambda (x) (display " ") (write x))
                  (cddr ls))
                 (display "]"))
                (else
                 (display " ")
                 (write (car ls))
                 (lp (cdr ls)))))
        (display ")")
        (cond
         ((pair? (cddr type))
          (display "<br />")
          (display "&nbsp;&nbsp;=> ")
          (write (caddr type)))))
       ((and (pair? type) (eq? 'syntax (car type)))
        (display "(<span class=\"keyword\">")
        (write sym)
        (display "</span>")
        (for-each (lambda (x) (display " ") (write x)) (cadr type))
        (display ")")))
      (cond
       ((and (pair? o) (string? (car o)))
        (display "<br /><i>")
        (display (car o))
        (display "</i>"))))))

(define (get-href sym)
  (let ((str (symbol->string sym)))
    (let lp ((x index))
      ;;(fprintf (current-error-port) "lp ~S\n" x)
      (match x
        (('a ('@ ('href href)) ('tt name ...))
         (and (every string? name)
              (equal? str (string-concatenate name))
              href))
        ((a . b) (or (lp a) (lp b)))
        (_ #f)))))

(display "(\n")
(for-each
 (lambda (x)
   (display "   ")
   (write (list (car x) (get-href (car x)) (html-escape (apply gen-doc x))))
   (newline))
 symbols)
(display ")\n")
