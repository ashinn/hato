;;; write-number.scm -- excert from unpublished formatting library
;;
;; Copyright (c) 2005 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; Two new procedures are provided:
;;
;;   write-number num [base digits sign commify? comma-count comma-sep dec-sep]
;;
;; Print number NUM to current-output-port with the following formatting
;; options:
;;
;;  BASE        - output base of number, from 2-36, default 10
;;  DIGITS      - number of fixed floating point digits, or #f for free format
;;  SIGN        - if #t print + for positive integers
;;                if a character, wrap negative numbers between that
;;                  character and it's mirror image, e.g. #\( prints
;;                  negative numbers in parenthesis: -3.14 => (3.14)
;;  COMMIFY?    - interleave groups of number with commas: 123,456,789.0
;;  COMMA-COUNT - number of digits to group with commands, default 3
;;  COMMA-SEP   - character or string to use for comma separation
;;  DEC-SEP     - character or string to use for decimal point separation

;; The procedure number->string* takes the same options as above but
;; returns the result as a string, and so is backwards compatible with
;; the R5RS procedure number->string.

;; Notes:
;;   SIGN is overloaded and should probably be split into two options.
;;   COMMA-COUNT should be extended to allow non-uniform counts as in the
;;     Indian convention of 3 then 2: 50,00,00,000.
;;   Digits above 9 are printed in uppercase letters A-Z, as in
;;     Common-Lisp.  Extending with an optional digit converter would
;;     not only allow us to make these lowercase, but would let us use
;;     other digits such as Han ideograms, or alternate encodings such
;;     as base64.

;; General algorithm based on "Printing Floating-Point Numbers Quickly
;; and Accurately" (FP-Printing-PLDI96.pdf).

(define-inline (imag-part x) 0)
(define-inline (real-part x) x)

(define-inline (quotient&remainder a b)
  (values (quotient a b) (remainder a b)))

(define (integer-log a base)
  (if (zero? a)
    0
    (inexact->exact (ceiling (/ (log (+ a 1)) (log base))))))
(define (integer-length a)
  (if (negative? a)
    (integer-log (- 1 a) 2)
    (integer-log a 2)))

(define invlog2of
  (let ((table (make-vector 37))
        (log2 (log 2)))
    (do ((b 2 (+ b 1)))
        ((= b 37))
      (vector-set! table b (/ log2 (log b))))
    (lambda (b)
      (if (<= 2 b 36)
        (vector-ref table b)
        (/ log2 (log b))))))

(define fast-expt
  (let ((table (make-vector 326)))
    (do ((k 0 (+ k 1)) (v 1 (* v 10)))
        ((= k 326))
      (vector-set! table k v))
    (lambda (b k)
      (if (and (= b 10) (<= 0 k 326))
        (vector-ref table k)
        (expt b k)))))

(define-constant *min-e* -1024)
(define-constant *bot-f* (expt 2 52))
(define-constant *top-f* (* 2 *bot-f*))

(define (write-number num . opt)

  (let-optionals* opt
      ((base 10)
       (digits #f)
       (sign? #f)
       (commify? #f)
       (comma-int 3)
       (comma-sep #\,)
       (decimal-sep #\.))

    (define (write-digit d)
      (let ((d (inexact->exact (truncate d))))
        (write-char
         (cond ((< d 10) (integer->char (+ d (char->integer #\0))))
               ((< d 36) (integer->char (+ (- d 10) (char->integer #\A))))
               (else (error "invalid digit: " d))))))

    (define (pad d i) ; just pad 0's, not #'s
      (write-digit d)
      (let lp ((i (- i 1)))
        (when (>= i 0)
          (if (= i (- digits 1))
            (display decimal-sep))
          (write-char #\0) (lp (- i 1)))))

    (define (write-positive num)

      (let* ((m+e (mantissa+exponent num))
             (f (car m+e))
             (e (cadr m+e))
             (inv-base (invlog2of base))
             (round? (even? f))
             (smaller (if round? <= <))
             (bigger (if round? >= >)))

        (define (scale r s m+ m- k f e)
          (let ((est (inexact->exact
                      (ceiling (- (* (+ e (integer-length f) -1)
                                     (invlog2of base))
                                  1.0e-10)))))
            (if (not (negative? est))
              (fixup r (* s (fast-expt base est)) m+ m- est)
              (let ((skale (fast-expt base (- est))))
                (fixup (* r skale) s (* m+ skale) (* m- skale) est)))))

        (define (fixup r s m+ m- k)
          (if (bigger (+ r m+) s)
            (lead r s m+ m- (+ k 1))
            (lead (* r base) s (* m+ base) (* m- base) k)))

        (define (lead r s m+ m- k)
          (let ((gen (if digits generate-fixed generate-all)))
            (unless (positive? k)
              (write-char #\0)
              (display decimal-sep)
              (let lp ((i 0))
                (when (> i k)
                  (write-char #\0)
                  (lp (- i 1)))))
            (gen r s m+ m- k)))

        (define (generate-all r s m+ m- k)
          (let gen ((r r) (m+ m+) (m- m-) (i k))
            (unless (= i k)
              (cond ((zero? i)
                     (display decimal-sep))
                    ((and commify? (positive? i) (zero? (modulo i comma-int)))
                     (display comma-sep))))
            (receive (d r) (quotient&remainder r s)
              (if (not (smaller r m-))
                (if (not (bigger (+ r m+) s))
                  (begin
                    (write-digit d)
                    (gen (* r base) (* m+ base) (* m- base) (- i 1)))
                  (write-digit (+ d 1)))
                (if (not (bigger (+ r m+) s))
                  (write-digit d)
                  (write-digit (if (< (* r 2) s) d (+ d 1))))))))

        (define (generate-fixed r s m+ m- k)
          (let ((i0 (- (+ k digits) 1)))
            (let gen ((r r) (m+ m+) (m- m-) (i i0))
              (unless (= i i0)
                (cond ((= i (- digits 1))
                       (display decimal-sep))
                      ((and commify? (> i (- digits 1))
                            (zero? (modulo (- i digits -1) comma-int)))
                       (display comma-sep))))
              (receive (d r) (quotient&remainder r s)
                (if (zero? i)
                  (write-digit (if (< (* r 2) s) d (+ d 1)))
                  (if (smaller r m-)
                    (if (bigger (+ r m+) s)
                      (pad (if (< (* r 2) s) d (+ d 1)) i)
                      (pad d i))
                    (if (bigger (+ r m+) s)
                      (pad (+ d 1) i)
                      (begin
                        (write-digit d)
                        (gen (* r base) (* m+ base)
                             (* m- base) (- i 1))))))))))

        (if (negative? e)
           (if (or (= e *min-e*) (not (= f *bot-f*)))
             (scale (* f 2) (* (expt 2 (- e)) 2) 1 1 0 f e)
             (scale (* f 2 2) (* (expt 2 (- 1 e)) 2) 2 1 0 f e))
           (if (= f *bot-f*)
             (let ((be (expt 2 e)))
               (scale (* f be 2) 2 be be 0 f e))
             (let* ((be (expt 2 e)) (be1 (* be 2)))
               (scale (* f be1 2) (* 2 2) be1 be 0 f e))))))

    (define (write-real num sign?)
      (cond
        ((negative? num)
         (if (char? sign?)
           (begin (display sign?) (write-positive (abs num))
                  (display (mirror-of sign?)))
           (begin (write-char #\-) (write-positive (abs num)))))
        (else
         (if (and sign? (not (char? sign?)))
           (write-char #\+))
         (write-positive num))))

    (let ((imag (imag-part num)))
      (if (zero? imag)
        (write-real num sign?)
        (begin (write-real imag sign?)
               (write-real (real-part num) #t)
               (write-char #\i))))))

(define (number->string* . args)
  (with-output-to-string (lambda () (apply write-number args))))

(define (mirror-of c)
  (case c ((#\() #\)) ((#\[) #\]) ((#\{) #\}) ((#\<) #\>) (else c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formatting helpers

;; Break a positive real number down to a normalized mantissa and
;; exponent. Default base=2, mant-size=52, exp-size=11 for IEEE doubles.
;;
;; Note: Can be implemented much faster in C, see decode-float in
;; ChezScheme or Gauche.
(define (mantissa+exponent num . opt)
  (if (zero? num)
    (list 0 0)
    (let-optionals* opt ((base 2) (mant-size 52) (exp-size 11))
      (let* ((bot (expt base mant-size))
             (top (* base bot)))
        (let loop ((n num) (e 0))
          (cond
            ((>= n top)
             (loop (quotient n base) (+ e 1)))
            ((< n bot)
             (loop (* n base) (- e 1)))
            (else
             (list (inexact->exact n) e))))))))

