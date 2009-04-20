;; dns.scm -- Domain Name Service library
;;
;; Copyright (c) 2005-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; Currently we just call out to dig, which is simple and fast enough.
;; The in-progress UDP-based interface is commented out below.

(module dns
 (dns-query dns-response dns-lookup dns-text dns-mx)

(import scheme chicken posix)

;(use udp lolevel srfi-4)

(define (escape-non-alpha-numeric+list str ls)
  (list->string
   (let lp ((ls (string->list str)) (res '()))
     (if (null? ls)
       (reverse res)
       (let ((c (car ls)))
         (if (or (char-alphabetic? c)
                 (char-numeric? c)
                 (memv c ls))
           (lp (cdr ls) (cons c res))
           (lp (cdr ls) (cons c (cons #\\ res)))))))))

(define (shell-escape str)
  (escape-non-alpha-numeric+list str '(#\. #\- #\_ #\/)))

;; dig +short synthcode.com
;; 66.93.216.137

;; adnshost +Do +Dt +Dc synthcode.com
;; INET 66.93.216.137

(define (dns-lookup domain)
  (let lp ((commands
            '("dig +short "
              ;;"adnshost +Do +Dt +Dc "
              ;;"perl -MNet::DNS -e 'sub g{print((Net::DNS::Resolver->new->search($_[0]))->{answer}[0]{address})}' -e 'g '"
              )))
    (and (pair? commands)
         (or
          (call-with-input-pipe
             (string-append (car commands) (shell-escape domain))
           (lambda (in)
             (let ((line (read-line in)))
               (and (not (eof-object? line))
                    (not (string=? line ""))
                    (if (and (> (string-length line) 5)
                             (string=? "INET " (substring line 0 5)))
                      (substring line 5)
                      line)))))
          (lp (cdr commands))))))

(define (dns-text domain)
  (call-with-input-pipe
     (string-append "dig +short " (shell-escape domain) " TXT")
   (lambda (in)
     (let ((line (read-line in)))
       (and (not (eof-object? line))
            (not (string=? line ""))
            line)))))

(define (normalize-domain str parent)
  (let ((len (string-length str)))
    (if (eqv? #\. (string-ref str (- len 1)))
      (substring str 0 (- len 1))
      (string-append parent "." str))))

(define (dns-mx domain)
  (call-with-input-pipe
   (string-append "dig +short " (shell-escape domain) " MX")
   (lambda (in)
     (let lp ((res '()))
       (let ((line (read-line in)))
         (if (eof-object? line)
           (sort res (lambda (a b) (< (car a) (car b))))
           (let* ((ls (string-split line)))
             (if (and (pair? (cdr ls)) (not (equal? "" (cadr ls))))
               (lp (cons (list (string->number (car ls))
                               (normalize-domain (cadr ls) domain))
                         res))
               (lp res)))))))))

(define (dns-query opcode questions)
  #f)

(define (dns-response opcode answers)
  #f)

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;; INCOMPLETE - need to get this working with direct UDP

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;; byte-vector utilities

; (define null-byte-vector (byte-vector))

; (define (byte-vector-append a b . o)
;   (define (append a b)
;     (let ((a-len (byte-vector-length a))
;           (b-len (byte-vector-length b)))
;       (cond
;         ((zero? a-len) b)
;         ((zero? b-len) a)
;         (else
;          (let ((c (make-byte-vector (+ a-len b-len))))
;            (move-memory! a c)
;            (let lp ((from 0) (to a-len))
;              (unless (= from b-len)
;                (byte-vector-set! c to (byte-vector-ref b from))
;                (lp (+ from 1) (+ to 1))))
;            c)))))
;   (let lp ((c (append a b)) (o o))
;     (if (pair? o)
;       (lp (append c (car o)) (cdr o))
;       c)))

; (define (byte-vector-append-map ls proc)
;   (if (pair? ls)
;     (if (pair? (cdr ls))
;       (apply byte-vector-append (map proc ls))
;       (proc (car ls)))
;     null-byte-vector))


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;; constants

; ;; types
; (define dns/A 1)
; (define dns/NS 2)
; (define dns/MD 3)
; (define dns/MF 4)
; (define dns/CNAME 5)
; (define dns/SOA 6)
; (define dns/MB 7)
; (define dns/MG 8)
; (define dns/MR 9)
; (define dns/NULL 10)
; (define dns/WKS 11)
; (define dns/PTR 12)
; (define dns/HINFO 13)
; (define dns/MINFO 14)
; (define dns/MX 15)
; (define dns/TXT 16)

; ;; qtypes
; (define dns/AXFR 252)
; (define dns/MAILB 253)
; (define dns/MAILA 254)
; (define dns/* 255)

; ;; classes
; (define dns/IN 1)
; (define dns/CS 2)
; (define dns/CH 3)
; (define dns/HS 4)

; (define dns-max-id (- (arithmetic-shift 1 16) 1))


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;; low-level interface

; (define-inline (bool->int n x) (if n (arithmetic-shift 1 x) 0))

; (define (dns-header id query? opcode authoritative? truncated?
;                     recursion-desired? recursion-available?
;                     response-code question-count answer-count
;                     authority-count additional-count)
;   (u16vector->byte-vector
;    (u16vector
;     id
;     (bitwise-ior (bool->int query? 15)
;                  (arithmetic-shift opcode 11)
;                  (bool->int authoritative? 10)
;                  (bool->int truncated? 9)
;                  (bool->int recursion-desired? 8)
;                  (bool->int recursion-desired? 7)
;                  response-code)
;     question-count
;     answer-count
;     authority-count
;     additional-count)))

; (define (dns-name-as-labels name)
;   (let ((len (string-length name)))
;     (if (<= len 63)
;       (string->byte-vector
;        (string-append (string (integer->char len)) name))
;       (byte-vector-append
;        (string-append "?" (substring name 0 63))
;        (dns-name-as-labels (substring name 63))))))

; (define (dns-question name type class)
;   (byte-vector-append
;    (dns-name-as-labels name)
;    (u16vector->byte-vector (u16vector type class))))

; (define (dns-rr name type class ttl rdata)
;   (byte-vector-append
;    (dns-name-as-labels name)
;    (u16vector->byte-vector
;     (u16vector type
;                class
;                (arithmetic-shift ttl -16)
;                (bitwise-and ttl (- (arithmetic-shift 1 16) 1))
;                (byte-vector-length rdata)))
;    rdata))

; (define (dns-message query? opcode response-code . o)
;   (let-optionals* o ((question-ls '())
;                      (answer-ls '())
;                      (authority-ls '())
;                      (additional-ls '())
;                      (authoritative? #t)
;                      (truncated? #f)
;                      (recursion-desired? #t)
;                      (recursion-available? #t)
;                      (id (random dns-max-id)))
;     (byte-vector-append
;      (dns-header id query? opcode authoritative? truncated?
;                  recursion-desired? recursion-available?
;                  response-code
;                  (length question-ls) (length answer-ls)
;                  (length authority-ls) (length additional-ls))
;      (byte-vector-append-map question-ls (cut apply dns-question <...>))
;      (byte-vector-append-map answer-ls (cut apply dns-rr <...>))
;      (byte-vector-append-map authority-ls (cut apply dns-rr <...>))
;      (byte-vector-append-map additional-ls (cut apply dns-rr <...>)))))


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;; high-level interface

; ;; XXXX Unix specific
; (define (default-dns-socket)
;   (with-input-from-file "/etc/resolv.conf"
;     (lambda ()
;       (let lp ()
;         (let ((line (read-line)))
;           (if (eof-object? line)
;             #f
;             (let ((ls (string-split line)))
;               (if (string=? (car ls) "nameserver")
;                 (let ((s (udp-open-socket)))
;                   (udp-bind! s #f 0)
;                   (udp-connect! s (cadr ls) 53))
;                 (lp)))))))))

; (define (dns-query opcode questions)
;   (dns-message #t opcode 0 questions))

; (define (dns-response opcode answers)
;   (dns-message #f 0 opcode '() answers))

; (define (dns-lookup name . o)
;   (let ((s (if (pair? o) (car o) (default-dns-socket))))
;     (udp-send s (byte-vector->string
;                  (dns-query dns/A (list (list name dns/* dns/IN)))))))

; (define (dns-text hostname)
;   #f)

; (define (dns-mx hostname)
;   #f)

 )
