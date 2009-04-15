;;;; hato-uri.scm -- URI parsing/building library
;;
;; Copyright (c) 2005-2008 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use extras)

(cond-expand
 ((and chicken compiling)
  (declare
   (export uri->string make-uri string->uri
           uri-scheme uri-user uri-host uri-port
           uri-path uri-query uri-fragment
           uri-with-scheme uri-with-user uri-with-host uri-with-path
           uri-with-query uri-with-fragment uri-with-port
           uri-encode uri-decode
           uri-query->alist uri-alist->query)))
 (else))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We reverse the order of the components since the more specific path
;; and query are likely to change much more frequently than the less
;; specific host and scheme.  Arguably a more shuffled ordering with
;; path at the car and query at the cadr may be more efficient.  This
;; mutation-free approach theoretically lets a webpage with 10,000
;; links to the same site be stored in memory as 10,000 cons cells
;; instead of, say, 10,000 7-element records.

(define (make-uri scheme . o)
  (let-optionals* o ((user #f) (host #f) (port #f)
                     (path #f) (query #f) (fragment #f))
    (list fragment query path port host user scheme)))

(define (uri-scheme uri) (caddr (cddddr uri)))
(define (uri-user uri) (cadr (cddddr uri)))
(define (uri-host uri) (car (cddddr uri)))
(define uri-port cadddr)
(define uri-path caddr)
(define uri-query cadr)
(define uri-fragment car)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string utils (don't feel like using SRFI-13 and these are more
;; specialised)

(define (string-scan str ch . o)
  (let-optionals* o ((start 0) (end (string-length str)))
    (let lp ((i start))
      (and (< i end)
           (if (eqv? ch (string-ref str i))
               i
               (lp (+ i 1)))))))

(define (string-scan-right str ch . o)
  (let-optionals* o ((start 0) (end (string-length str)))
    (let lp ((i (- end 1)))
      (and (>= i start)
           (if (eqv? ch (string-ref str i))
               i
               (lp (- i 1)))))))

(define (string-downcase->symbol str)
  (let ((len (string-length str)))
    (let lp ((i 0))
      (cond
       ((= i len)
        (string->symbol str))
       ((char-upper-case? (string-ref str i))
        (let ((res (make-string len)))
          (do ((j 0 (+ j 1)))
              ((= j i))
            (string-set! res j (string-ref str j)))
          (string-set! res i (char-downcase (string-ref str i)))
          (do ((j (+ i 1) (+ j 1)))
              ((= j len))
            (string-set! res j (char-downcase (string-ref str j))))
          (string->symbol res)))
       (else
        (lp (+ i 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functional updaters (uses as much shared state as possible)

(define (uri-updater proc)
  (lambda (uri new-value)
    (if (string? uri)
        (uri->string (proc (uri->string uri) new-value))
        (proc uri new-value))))

(define uri-with-scheme
  (uri-updater
   (lambda (uri scheme)
     (list (car uri) (cadr uri) (caddr uri) (cadddr uri)
           (uri-host uri) (uri-user uri) scheme))))
(define uri-with-user
  (uri-updater
   (lambda (uri user)
     (list (car uri) (cadr uri) (caddr uri) (cadddr uri)
           (uri-host uri) user (uri-scheme uri)))))
(define uri-with-host
  (uri-updater
   (lambda (uri host)
     (cons (car uri)
           (cons (cadr uri)
                 (cons (caddr uri)
                       (cons (cadddr uri) (cons host (cdddr uri)))))))))
(define uri-with-port
  (uri-updater
   (lambda (uri port)
     (cons (car uri)
           (cons (cadr uri) (cons (caddr uri) (cons port (cdddr uri))))))))
(define uri-with-path
  (uri-updater
   (lambda (uri path) (cons (car uri) (cons (cadr uri) (cons path (cdddr uri)))))))
(define uri-with-query
  (uri-updater (lambda (uri query) (cons (car uri) (cons query (cddr uri))))))
(define uri-with-fragment
  (uri-updater (lambda (uri fragment) (cons fragment (cdr uri)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parsing - without :// we just split into scheme & path

(define (string->uri str . o)
  (define decode? (and (pair? o) (car o)))
  (define decode (if decode? uri-decode identity))
  (define decode-query
    (if (and (pair? o) (pair? (cdr o)) (cadr o))
        uri-query->alist
        decode))
  (if (pair? str)
      str
      (let ((len (string-length str))
            (colon (string-scan str #\:)))
        (if (or (not colon) (zero? colon))
            #f
            (let ((sc1 (+ colon 1))
                  (scheme (string-downcase->symbol (substring str 0 colon))))
              (if (= sc1 len)
                  (make-uri scheme)
                  (if (or (>= (+ sc1 1) len)
                          (not (and (eqv? #\/ (string-ref str sc1))
                                    (eqv? #\/ (string-ref str (+ sc1 1))))))
                      (make-uri scheme #f #f #f (substring str sc1 len))
                      (if (>= (+ sc1 2) len)
                          (make-uri scheme #f "")
                          (let* ((sc2 (+ sc1 2))
                                 (slash (string-scan str #\/ sc2))
                                 (sc3 (or slash len))
                                 (at (string-scan-right str #\@ sc2 sc3))
                                 (colon3 (string-scan str #\: (or at sc2) sc3))
                                 (quest (string-scan str #\? sc3))
                                 (pound (string-scan str #\# (or quest sc3))))
                            (list (and pound
                                       (decode (substring str (+ pound 1))))
                                  (and quest
                                       (decode-query
                                        (substring str (+ quest 1)
                                                   (or pound len))))
                                  (and
                                   slash
                                   (decode
                                    (substring str slash (or quest pound len))))
                                  (and colon3
                                       (string->number
                                        (substring str (+ colon3 1) sc3)))
                                  (decode
                                   (substring
                                    str
                                    (if at (+ at 1) sc2)
                                    (or colon3 sc3)))
                                  (and at (decode (substring str sc2 at)))
                                  scheme))))))))))

(define (uri->string uri . o)
  (define encode? (and (pair? o) (car o)))
  (define encode (if encode? uri-encode identity))
  (if (string? uri)
      uri
      (apply
       (lambda (fragment query path port host user scheme)
         (string-append
          (symbol->string scheme) ":"
          (if (or user host port) "//" "")
          (if user (encode user) "") (if user "@" "")
          (or host "") ; host shouldn't need encoding
          (if port ":" "") (if port (number->string port) "")
          (if path (encode path) "")
          (if query "?" "")
          (if (pair? query) (uri-alist->query query) (or query ""))
          (if fragment "#" "") (if fragment (encode fragment) "")))
       uri)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; query encoding and decoding

(define (uri-safe-char? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (case ch
        ((#\- #\_ #\. #\! #\~ #\* #\' #\( #\)) #t)
        (else #f))))

(define (collect str from to res)
  (if (>= from to)
      res
      (cons (substring str from to) res)))

(define (uri-encode str . o)
  (define (encode-1-space ch)
    (if (eqv? ch #\space)
        "+"
        (encode-1-normal ch)))
  (define (encode-1-normal ch)
    (let* ((i (char->integer ch))
           (hex (number->string i 16)))
      (if (< i 16)
          (string-append "%0" hex)
          (string-append "%" hex))))
  (let ((start 0)
        (end (string-length str))
        (encode-1 (if (and (pair? o) (car o))
                      encode-1-space
                      encode-1-normal)))
    (let lp ((from start) (to start) (res '()))
      (if (>= to end)
          (if (zero? from)
              str
              (string-intersperse (reverse (collect str from to res)) ""))
          (let* ((ch (string-ref str to))
                 (next (+ to 1)))
            (if (uri-safe-char? ch)
                (lp from next res)
                (lp next next (cons (encode-1 ch)
                                    (collect str from to res)))))))))

(define (uri-decode str . o)
  (let ((space-as-plus? (and (pair? o) (car o)))
        (start 0)
        (end (string-length str)))
    (let lp ((from start) (to start) (res '()))
      (if (>= to end)
          (if (zero? from)
              str
              (string-intersperse (reverse (collect str from to res)) ""))
          (let* ((ch (string-ref str to))
                 (next (+ to 1)))
            (cond
             ((eqv? ch #\%)
              (if (>= next end)
                  (lp next next (collect str from to res))
                  (let ((next2 (+ next 1)))
                    (if (>= next2 end)
                        (lp next2 next2 (collect str from to res))
                        (let* ((next3 (+ next2 1))
                               (hex (substring str next next3))
                               (i (string->number hex 16)))
                          (lp next3 next3 (cons (string (integer->char i))
                                                (collect str from to res))))))))
             ((and space-as-plus? (eqv? ch #\+))
              (lp next next (cons " " (collect str from to res))))
             (else
              (lp from next res))))))))

(define (uri-query->alist str . o)
  ((lambda (res)
     (let ((plus? (and (pair? o) (car o))))
       (map (lambda (x)
              (cons (uri-decode (car x) plus?)
                    (and (cdr x) (uri-decode (cdr x) plus?))))
            res)))
   (map (lambda (x)
          (let ((sc (string-scan x #\=)))
            (if sc
                (cons (substring x 0 sc)
                      (substring x (+ sc 1)))
                (cons x #f))))
        (string-split str "&;"))))

(define (uri-alist->query ls . o)
  (let ((plus? (and (pair? o) (car o))))
    (string-intersperse
     (map
      (lambda (x)
        (let ((key (uri-encode (car x) plus?)))
          (if (cdr x)
              (string-append key "=" (uri-encode (cdr x) plus?))
              key)))
      ls)
     "&")))
