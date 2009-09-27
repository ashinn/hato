;; dns.scm -- Domain Name Service library
;;
;; Copyright (c) 2005-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; see http://tools.ietf.org/html/rfc1035

(require-extension udp lolevel srfi-4)

(module dns
 (dns-lookup dns-text dns-mx dns-lookup/full dns-text/full dns-mx/full)

(import scheme chicken extras data-structures posix ports udp lolevel srfi-4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; u8vector utilities

(define (u8vector-append a . o)
  (define (append a b)
    (let ((a-len (u8vector-length a))
          (b-len (u8vector-length b)))
      (cond
       ((zero? a-len) b)
       ((zero? b-len) a)
       (else
        (let ((c (make-u8vector (+ a-len b-len))))
          (move-memory! a c)
          (let lp ((from 0) (to a-len))
            (unless (= from b-len)
              (u8vector-set! c to (u8vector-ref b from))
              (lp (+ from 1) (+ to 1))))
          c)))))
  (let lp ((res a) (ls o))
    (if (pair? ls)
        (lp (append res (car ls)) (cdr ls))
        res)))

(define (u8vector-append-map ls proc)
  (if (pair? ls)
      (if (pair? (cdr ls))
          (apply u8vector-append (map proc ls))
          (proc (car ls)))
      '#u8()))

(define (u16vector->u8vector/be u16)
  (let* ((len (u16vector-length u16))
         (res (make-u8vector (* len 2))))
    (do ((i 0 (+ i 1)))
        ((= i len) res)
      (let ((v (u16vector-ref u16 i)))
        (u8vector-set! res (* i 2) (arithmetic-shift v -8))
        (u8vector-set! res (+ (* i 2) 1) (bitwise-and v #xFF))))))

(define (string->u8vector str)
  (blob->u8vector (string->blob str)))

(define (u8vector->string str)
  (blob->string (u8vector->blob str)))

(define (string-scan ch str . o)
  (let ((end (string-length str)))
    (let scan ((i (if (pair? o) (car o) 0)))
      (cond ((>= i end) #f)
            ((eq? ch (string-ref str i)) i)
            (else (scan (+ i 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

;; types
(define dns/A 1)
(define dns/NS 2)
(define dns/MD 3)
(define dns/MF 4)
(define dns/CNAME 5)
(define dns/SOA 6)
(define dns/MB 7)
(define dns/MG 8)
(define dns/MR 9)
(define dns/NULL 10)
(define dns/WKS 11)
(define dns/PTR 12)
(define dns/HINFO 13)
(define dns/MINFO 14)
(define dns/MX 15)
(define dns/TXT 16)

;; qtypes
(define dns/AXFR 252)
(define dns/MAILB 253)
(define dns/MAILA 254)
(define dns/* 255)

;; classes
(define dns/IN 1)
(define dns/CS 2)
(define dns/CH 3)
(define dns/HS 4)

;; opcodes
(define dns/QUERY 0)
(define dns/IQUERY 1)
(define dns/STATUS 2)

(define dns-max-id (- (arithmetic-shift 1 16) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; low-level interface

(define (bool->int n x) (if n (arithmetic-shift 1 x) 0))

(define (dns-header id query? opcode authoritative? truncated?
                    recursion-desired? recursion-available?
                    response-code question-count answer-count
                    authority-count additional-count)
  (let ((flags
         (bitwise-ior (bool->int (not query?) 15)
                      (arithmetic-shift opcode 11)
                      (bool->int authoritative? 10)
                      (bool->int truncated? 9)
                      (bool->int recursion-desired? 8)
                      (bool->int recursion-available? 7)
                      response-code)))
    (u16vector->u8vector/be
     (u16vector id flags question-count answer-count
                authority-count additional-count))))

(define (dns-name-as-labels name)
  (string->u8vector
   (string-append
    (let lp ((start 0))
      (let* ((i (string-scan #\. name start))
             (end (or i (string-length name))))
        (string-append
         (string (integer->char (- end start)))
         (substring name start end)
         (if i (lp (+ i 1)) ""))))
    (string (integer->char 0)))))

(define (dns-question name type class)
  (u8vector-append (dns-name-as-labels name)
                   (u16vector->u8vector/be (u16vector type class))))

(define (dns-rr name type class ttl rdata)
  (u8vector-append
   (dns-name-as-labels name)
   (u16vector->u8vector/be
    (u16vector type
               class
               (arithmetic-shift ttl -16)
               (bitwise-and ttl (- (arithmetic-shift 1 16) 1))
               (u8vector-length rdata)))
   rdata))

(define (dns-message query? opcode response-code . o)
  (let-optionals* o ((question-ls '())
                     (answer-ls '())
                     (authority-ls '())
                     (additional-ls '())
                     (authoritative? #f)
                     (truncated? #f)
                     (recursion-desired? #t)
                     (recursion-available? #f)
                     (id (random dns-max-id)))
    (u8vector->string
     (u8vector-append
      (dns-header id query? opcode authoritative? truncated?
                  recursion-desired? recursion-available?
                  response-code
                  (length question-ls) (length answer-ls)
                  (length authority-ls) (length additional-ls))
      (u8vector-append-map question-ls (cut apply dns-question <...>))
      (u8vector-append-map answer-ls (cut apply dns-rr <...>))
      (u8vector-append-map authority-ls (cut apply dns-rr <...>))
      (u8vector-append-map additional-ls (cut apply dns-rr <...>))))))

(define (dns-build-query opcode questions)
  (dns-message #t opcode 0 questions))

(define (dns-build-response opcode response answers)
  (dns-message #f opcode response '() answers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parsing

(define (read-length-string str off len)
  (let* ((start (car off))
         (end (+ start len)))
    (set-car! off end)
    (substring str start end)))

(define (read-uint8 str off)
  (let ((ch (string-ref str (car off))))
    (set-car! off (+ 1 (car off)))
    (char->integer ch)))

(define (read-uint16/be str off)
  (let* ((b1 (read-uint8 str off))
         (b2 (read-uint8 str off)))
    (bitwise-ior (arithmetic-shift b1 8) b2)))

(define (read-label str off)
  (let lp ((res '()))
    (let ((i (read-uint8 str off)))
      (cond
       ((or (eof-object? i) (zero? i))
        (string-intersperse (reverse res) "."))
       ((> i 63)
        (let* ((j (read-uint8 str off))
               (off2 (list (bitwise-ior
                           (arithmetic-shift (bitwise-and i #b111111) 8)
                           j))))
          (string-intersperse (reverse (cons (read-label str off2) res)) ".")))
       (else
        (lp (cons (read-length-string str off i) res)))))))

(define (read-question str off)
  (let* ((qname (read-label str off))
         (qtype (read-uint16/be str off))
         (qclass (read-uint16/be str off)))
    (list qname qtype qclass)))

(define (inet-ntoa n)
  (string-append (number->string (char->integer (string-ref n 0))) "."
                 (number->string (char->integer (string-ref n 1))) "."
                 (number->string (char->integer (string-ref n 2))) "."
                 (number->string (char->integer (string-ref n 3)))))

(define (parse-rdata str index rdata type class)
  (condition-case
      (cond
       ((and (eq? type dns/A) (= 4 (string-length rdata)))
        (inet-ntoa rdata))
       ((eq? type dns/NS)
        (read-label str (list index)))
       ((eq? type dns/MX)
        (let* ((preference (read-uint16/be rdata (list 0)))
               (host (read-label str (list (+ index 2)))))
          (list preference host)))
       (else
        ;;(if (not (eq? type dns/TXT))
        ;;    (fprintf (current-error-port) "unknown type: ~S ~S: ~S\n"
        ;;             type class rdata))
        rdata))
    (exn ()
         ;;(fprintf (current-error-port) "error reading: ~S ~S: ~S\n"
         ;;         type class rdata)
         ;;(print-error-message exn)
         rdata)))

(define (read-resource str off)
  (let* ((name (read-label str off))
         (type (read-uint16/be str off))
         (class (read-uint16/be str off))
         (ttl-hi (read-uint16/be str off))
         (ttl-lo (read-uint16/be str off))
         (rdlength (read-uint16/be str off))
         (data-index (car off))
         (raw-rdata (read-length-string str off rdlength))
         (rdata (parse-rdata str data-index raw-rdata type class)))
    (list name type class (list ttl-hi ttl-lo) rdata)))

(define (read-n reader n str off)
  (let lp ((n n) (res '()))
    (if (zero? n)
        (reverse res)
        (lp (- n 1) (cons (reader str off) res)))))

(define (dns-parse str)
  (let* ((off (list 0))
         (id (read-uint16/be str off))
         (tmp1 (read-uint8 str off))
         (tmp2 (read-uint8 str off))
         (num-questions (read-uint16/be str off))
         (num-answers (read-uint16/be str off))
         (num-name-servers (read-uint16/be str off))
         (num-additional (read-uint16/be str off))
         (questions (read-n read-question num-questions str off))
         (answers (read-n read-resource num-answers str off))
         (name-servers (read-n read-resource num-name-servers str off))
         (additional (read-n read-resource num-additional str off)))
    (list id tmp1 tmp2 questions answers name-servers additional)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; high-level interface

;; XXXX Unix specific
(define (dns-read-resolv-conf)
  (call-with-input-file "/etc/resolv.conf"
    (lambda (in)
      (let lp ((res '()))
        (let ((line (read-line in)))
          (if (eof-object? line)
              (reverse res)
              (let ((ls (string-split line)))
                (if (string=? (car ls) "nameserver")
                    (lp (cons (cadr ls) res))
                    (lp res)))))))))

(define dns-default-name-servers
  (let ((name-servers #f))
    (lambda ()
      (or name-servers
          (let ((ns (dns-read-resolv-conf)))
            (set! name-servers ns)
            ns)))))

(define (call-with-dns-socket host port proc . o)
  (let ((s (or (and (pair? o) (car o)) (udp-open-socket))))
    (udp-bind! s #f 0)
    (udp-connect! s host port)
    (let ((res (proc s)))
      (if (not (and (pair? o) (car o))) (udp-close-socket s))
      res)))

(define (dns-query-all msg . o)
  (let query ((ns-ls (if (pair? o) (list (car o)) (dns-default-name-servers))))
    (and (pair? ns-ls)
         (condition-case
             (call-with-dns-socket (car ns-ls) 53
               (lambda (s)
                 (udp-send s msg)
                 (receive (len str) (udp-recv s 1024) str)))
           (exn () (query (cdr ns-ls)))))))

(define (dns-run msg o)
  (dns-parse (apply dns-query-all msg o)))

(define (dns-lookup/full name . o)
  (dns-run (dns-build-query dns/QUERY (list (list name dns/A dns/IN))) o))
(define (dns-text/full name . o)
  (dns-run (dns-build-query dns/QUERY (list (list name dns/TXT dns/IN))) o))
(define (dns-mx/full name . o)
  (dns-run (dns-build-query dns/QUERY (list (list name dns/MX dns/IN))) o))

(define (dns-get-answer x)
  (and (list? x)
       (>= (length x) 5)
       (let ((ans-ls (list-ref x 4)))
         (and (pair? ans-ls)
              (list-ref (car ans-ls) 4)))))

(define (dns-lookup name . o)
  (dns-get-answer (apply dns-lookup/full name o)))
(define (dns-text name . o)
  (dns-get-answer (apply dns-text/full name o)))
(define (dns-mx name . o)
  (dns-get-answer (apply dns-mx/full name o)))

 )
