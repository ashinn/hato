;;;; hato-md5.scm -- md5 implementation
;;
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(require-library numbers)

(module hato-md5
  (md5-digest)

(import (except scheme + - * / = > < >= <= number->string string->number
                eqv? equal? exp log sin cos tan atan acos asin expt sqrt
                quotient modulo remainder numerator denominator abs max min
                gcd lcm positive? negative? zero? odd? even? exact? inexact?
                floor ceiling truncate round inexact->exact exact->inexact
                number? complex? real? rational? integer? real-part imag-part
                magnitude)
        chicken extras numbers)

(define (u32 a) (bitwise-and a #xFFFFFFFF))
(define (u32+ a b) (u32 (+ a b)))
(define (u32<<< n s)
  (bitwise-ior (arithmetic-shift n s) (arithmetic-shift n (- s 32))))

(define (extract-byte n i)
  (bitwise-and #xFF (arithmetic-shift n (* i -8))))

(define (string-u32-ref str i)
  (+ (char->integer (string-ref str i))
     (arithmetic-shift (char->integer (string-ref str (+ i 1))) 8)
     (arithmetic-shift (char->integer (string-ref str (+ i 2))) 16)
     (arithmetic-shift (char->integer (string-ref str (+ i 3))) 24)))

(define (string-byte-set! str i n)
  (string-set! str i (integer->char n)))

(define (hex n)
  (if (< n 16)
      (string-append "0" (number->string n 16))
      (number->string n 16)))

(define (number->u32-string n)
  (string-append
   (hex (extract-byte n 0)) (hex (extract-byte n 1))
   (hex (extract-byte n 2)) (hex (extract-byte n 3))))

;; 3. MD5 Algorithm Description

;;    We begin by supposing that we have a b-bit message as input, and that
;;    we wish to find its message digest. Here b is an arbitrary
;;    nonnegative integer; b may be zero, it need not be a multiple of
;;    eight, and it may be arbitrarily large. We imagine the bits of the
;;    message written down as follows:

;;           m_0 m_1 ... m_{b-1}

;;    The following five steps are performed to compute the message digest
;;    of the message.

;; 3.1 Step 1. Append Padding Bits

;;    The message is "padded" (extended) so that its length (in bits) is
;;    congruent to 448, modulo 512. That is, the message is extended so
;;    that it is just 64 bits shy of being a multiple of 512 bits long.
;;    Padding is always performed, even if the length of the message is
;;    already congruent to 448, modulo 512.

;;    Padding is performed as follows: a single "1" bit is appended to the
;;    message, and then "0" bits are appended so that the length in bits of
;;    the padded message becomes congruent to 448, modulo 512. In all, at
;;    least one bit and at most 512 bits are appended.

;; 3.2 Step 2. Append Length

;;    A 64-bit representation of b (the length of the message before the
;;    padding bits were added) is appended to the result of the previous
;;    step. In the unlikely event that b is greater than 2^64, then only
;;    the low-order 64 bits of b are used. (These bits are appended as two
;;    32-bit words and appended low-order word first in accordance with the
;;    previous conventions.)

;;    At this point the resulting message (after padding with bits and with
;;    b) has a length that is an exact multiple of 512 bits. Equivalently,
;;    this message has a length that is an exact multiple of 16 (32-bit)
;;    words. Let M[0 ... N-1] denote the words of the resulting message,
;;    where N is a multiple of 16.

;; 3.3 Step 3. Initialize MD Buffer

;;    A four-word buffer (A,B,C,D) is used to compute the message digest.
;;    Here each of A, B, C, D is a 32-bit register. These registers are
;;    initialized to the following values in hexadecimal, low-order bytes
;;    first):

;;           word A: 01 23 45 67
;;           word B: 89 ab cd ef
;;           word C: fe dc ba 98
;;           word D: 76 54 32 10

;; 3.4 Step 4. Process Message in 16-Word Blocks

;;    We first define four auxiliary functions that each take as input
;;    three 32-bit words and produce as output one 32-bit word.

;;           F(X,Y,Z) = XY v not(X) Z
;;           G(X,Y,Z) = XZ v Y not(Z)
;;           H(X,Y,Z) = X xor Y xor Z
;;           I(X,Y,Z) = Y xor (X v not(Z))

(define (FF X Y Z)
  (bitwise-ior (bitwise-and X Y) (bitwise-and (bitwise-not X) Z)))
(define (GG X Y Z)
  (bitwise-ior (bitwise-and X Z) (bitwise-and Y (bitwise-not Z))))
(define (HH X Y Z)
  (bitwise-xor X Y Z))
(define (II X Y Z)
  (bitwise-xor Y (bitwise-ior X (bitwise-not Z))))

(define T
  (do ((i 64 (- i 1))
       (ls '()
           (cons (u32 (inexact->exact (truncate (* 4294967296 (abs (sin i))))))
                 ls)))
      ((< i 0) (list->vector ls))))

;;    In each bit position F acts as a conditional: if X then Y else Z.
;;    The function F could have been defined using + instead of v since XY
;;    and not(X)Z will never have 1's in the same bit position.) It is
;;    interesting to note that if the bits of X, Y, and Z are independent
;;    and unbiased, the each bit of F(X,Y,Z) will be independent and
;;    unbiased.

;;    The functions G, H, and I are similar to the function F, in that they
;;    act in "bitwise parallel" to produce their output from the bits of X,
;;    Y, and Z, in such a manner that if the corresponding bits of X, Y,
;;    and Z are independent and unbiased, then each bit of G(X,Y,Z),
;;    H(X,Y,Z), and I(X,Y,Z) will be independent and unbiased. Note that
;;    the function H is the bit-wise "xor" or "parity" function of its
;;    inputs.

;;    This step uses a 64-element table T[1 ... 64] constructed from the
;;    sine function. Let T[i] denote the i-th element of the table, which
;;    is equal to the integer part of 4294967296 times abs(sin(i)), where i
;;    is in radians. The elements of the table are given in the appendix.

(define (md5-digest src)
  (let ((in (if (string? src) (open-input-string src) src))
        (buf (make-string 64))
        (vec (make-vector 16))
        (A #x67452301)
        (B #xefcdab89)
        (C #x98badcfe)
        (D #x10325476))
    ;; Process each 16-word block.
    (let lp ((i 0) (pad (integer->char #x80)))
      (let ((n (read-string! 64 buf in)))
        (cond
         ((< n 64)  ;; pad
          (let ((len (* 8 (+ i n))))
            (string-set! buf n pad)
            (do ((j (+ n 1) (+ j 1))) ((>= j 64))
              (string-set! buf j (integer->char 0)))
            (cond
             ((< n 56)
              (string-byte-set! buf 56 (extract-byte len 0))
              (string-byte-set! buf 57 (extract-byte len 1))
              (string-byte-set! buf 58 (extract-byte len 2))
              (string-byte-set! buf 59 (extract-byte len 3))
              (string-byte-set! buf 60 (extract-byte len 4))
              (string-byte-set! buf 61 (extract-byte len 5))
              (string-byte-set! buf 62 (extract-byte len 6))
              (string-byte-set! buf 63 (extract-byte len 7)))))))
        ;; Copy block i into X.
        (do ((j 0 (+ j 1)))
            ((= j 16))
          (vector-set! vec j (string-u32-ref buf (* j 4))))
        ;; Save A as AA, B as BB, C as CC, and D as DD.
        (let ((AA A)
              (BB B)
              (CC C)
              (DD D))
          (letrec-syntax
              ((R
                (syntax-rules ()
                  ((R op a b c d k s i)
                   (set! a (u32+ b (u32<<< (u32+ (u32+ a (op b c d))
                                                 (u32+ (vector-ref vec k)
                                                       (vector-ref T i)))
                                           s))))))
               (R1 (syntax-rules () ((R1 a b c d k s i) (R FF a b c d k s i))))
               (R2 (syntax-rules () ((R2 a b c d k s i) (R GG a b c d k s i))))
               (R3 (syntax-rules () ((R3 a b c d k s i) (R HH a b c d k s i))))
               (R4 (syntax-rules () ((R4 a b c d k s i) (R II a b c d k s i)))))
            ;; Round 1: Let [abcd k s i] denote the operation
            ;;   a = b + ((a + F(b,c,d) + X[k] + T[i]) <<< s)
            (R1 A B C D 0 7 1)     (R1 D A B C 1 12 2)
            (R1 C D A B 2 17 3)    (R1 B C D A 3 22 4)
            (R1 A B C D 4 7 5)     (R1 D A B C 5 12 6)
            (R1 C D A B 6 17 7)    (R1 B C D A 7 22 8)
            (R1 A B C D 8 7 9)     (R1 D A B C 9 12 10)
            (R1 C D A B 10 17 11)  (R1 B C D A 11 22 12)
            (R1 A B C D 12 7 13)   (R1 D A B C 13 12 14)
            (R1 C D A B 14 17 15)  (R1 B C D A 15 22 16)
            ;; Round 2: Let [abcd k s i] denote the operation
            ;;   a = b + ((a + G(b,c,d) + X[k] + T[i]) <<< s)
            (R2 A B C D 1 5 17)    (R2 D A B C 6 9 18)
            (R2 C D A B 11 14 19)  (R2 B C D A 0 20 20)
            (R2 A B C D 5 5 21)    (R2 D A B C 10 9 22)
            (R2 C D A B 15 14 23)  (R2 B C D A 4 20 24)
            (R2 A B C D 9 5 25)    (R2 D A B C 14 9 26)
            (R2 C D A B 3 14 27)   (R2 B C D A 8 20 28)
            (R2 A B C D 13 5 29)   (R2 D A B C 2 9 30)
            (R2 C D A B 7 14 31)   (R2 B C D A 12 20 32)
            ;; Round 3: Let [abcd k s i] denote the operation
            ;;   a = b + ((a + H(b,c,d) + X[k] + T[i]) <<< s)
            (R3 A B C D 5 4 33)    (R3 D A B C 8 11 34)
            (R3 C D A B 11 16 35)  (R3 B C D A 14 23 36)
            (R3 A B C D 1 4 37)    (R3 D A B C 4 11 38)
            (R3 C D A B 7 16 39)   (R3 B C D A 10 23 40)
            (R3 A B C D 13 4 41)   (R3 D A B C 0 11 42)
            (R3 C D A B 3 16 43)   (R3 B C D A 6 23 44)
            (R3 A B C D 9 4 45)    (R3 D A B C 12 11 46)
            (R3 C D A B 15 16 47)  (R3 B C D A 2 23 48)
            ;; Round 4: Let [abcd k s i] denote the operation
            ;;   a = b + ((a + I(b,c,d) + X[k] + T[i]) <<< s)
            (R4 A B C D 0 6 49)    (R4 D A B C 7 10 50)
            (R4 C D A B 14 15 51)  (R4 B C D A 5 21 52)
            (R4 A B C D 12 6 53)   (R4 D A B C 3 10 54)
            (R4 C D A B 10 15 55)  (R4 B C D A 1 21 56)
            (R4 A B C D 8 6 57)    (R4 D A B C 15 10 58)
            (R4 C D A B 6 15 59)   (R4 B C D A 13 21 60)
            (R4 A B C D 4 6 61)    (R4 D A B C 11 10 62)
            (R4 C D A B 2 15 63)   (R4 B C D A 9 21 64)
            ;; Then in increment each of the four registers by the
            ;; value it had before this block was started.
            (set! A (u32+ A AA))
            (set! B (u32+ B BB))
            (set! C (u32+ C CC))
            (set! D (u32+ D DD))
            (cond
             ((< n 64)
              (if (>= n 56)
                  (lp (+ i n) (integer->char 0))
                  (string-append
                   (number->u32-string A) (number->u32-string B)
                   (number->u32-string C) (number->u32-string D))))
             (else
              (lp (+ i 64) pad)))))))))

;; 3.5 Step 5. Output

;;    The message digest produced as output is A, B, C, D. That is, we
;;    begin with the low-order byte of A, and end with the high-order byte
;;    of D.

;;    This completes the description of MD5. A reference implementation in
;;    C is given in the appendix.

)

