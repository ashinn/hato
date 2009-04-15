;; hato-spf.scm
;;
;; Copyright (c) 2005-2008 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(use dns lru-cache srfi-13 hato-mime)

(define hato-spf-cache (make-lru-cache equal: string=?))

;; "v=spf1 a:mproxy.gmail.com a:rproxy.gmail.com a:wproxy.gmail.com a:zproxy.gmail.com a:nproxy.gmail.com a:uproxy.gmail.com a:xproxy.gmail.com a:qproxy.gmail.com ?all"

(define (hato-spf-rule domain)
  (define (default-rule c)
    (case c ((#\-) 'reject) ((#\?) 'maybe) (else 'accept)))
  (define (verify-mx addr helo from msg hdr)
    #t)
  (define (verify-ptr addr helo from msg hdr)
    #t)
  (define (verify-exists addr helo from msg hdr)
    #t)
  (define (verify-a domain)
    (lambda (addr helo from msg hdr)
      #t))
  (let ((txt (dns-text domain)))
    (and
     (not (string-null? txt))
     (let lp ((ls (map string-trim-both (string-split txt)))
              (version #f)
              (default #f)
              (res '()))
       (if (null? ls)
         (and version (list version default res))
         (let ((x (car ls)))
           (cond
             ((string-prefix-ci? "v=spf" x)
              (lp (cdr ls) (string->number (substring x 5)) default res))
             ((and (= 4 (string-length x)) (string-suffix-ci? x "all"))
              (lp (cdr ls) version (default-rule (string-ref x 0)) res))
             (else
              (let ((ls2 (string-split x ":")))
                (case (string->symbol (string-downcase (car ls2)))
                  ((mx)
                   (lp (cdr ls) version default (cons verify-mx res)))
                  ((ptr)
                   (lp (cdr ls) version default (cons verify-ptr res)))
                  ((exists)
                   (lp (cdr ls) version default (cons verify-exists res)))
                  ((a)
                   (lp (cdr ls) version default
                       (if (pair? (cdr ls))
                         (cons (cut verify-a (cadr ls)) res)
                         res)))
                  ;;((ipv4) (lp (cdr ls) version default res))
                  ;;((ipv6) (lp (cdr ls) version default res))
                  (else
                   (lp (cdr ls) version default res))))))))))))

(define (hato-spf-verify? tcp-address helo-domain from-domain msg . o)
  (let ((rule (lru-ref! hato-spf-cache from-domain hato-spf-rule)))
    (or (not (pair? rule))
        (not (= 1 (car rule)))
        (let ((headers (if (pair? o)
                         (car o)
                         (with-input-from-string msg mime-headers->list))))
          (let lp ((ls (caddr rule)))
            (if (null? ls)
              (case (cadr rule) ; XXXX make these configurable
                ((reject) #f)
                ((maybe) #f)
                ((accept) #t)
                (else #t))
              (and ((car ls) tcp-address helo-domain from-domain msg headers)
                   (lp (cdr ls)))))))))
