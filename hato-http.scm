;;;; hato-http.scm -- http client
;;
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(require-library tcp autoload hato-uri hato-mime)

(module hato-http

(http-get call-with-input-url with-input-from-url
 http-parse-request http-parse-form)

(import scheme chicken extras ports data-structures tcp)
(import hato-uri hato-mime autoload)
(autoload openssl ssl-connect)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; client utils

(define http-user-agent "hato")

(define http-redirect-limit 10)
(define http-chunked-buffer-size 4096)
(define http-chunked-size-limit 409600)

(define (string-scan str ch . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
    (let lp ((i start))
      (and (< i end)
           (if (eqv? ch (string-ref str i))
               i
               (lp (+ i 1)))))))

(define (http-parse-response line)
  (let* ((len (string-length line))
         (i (or (string-scan line #\space 0 len) len))
         (j (or (string-scan line #\space (+ i 1) len) len))
         (n (and (< i j) (string->number (substring line (+ i 1) j)))))
    (if (not (integer? n))
        (error "bad response" line i j)
        (list (substring line 0 i)
              n
              (if (>= j len) "" (substring line (+ j 1) len))))))

(define (http-wrap-chunked-input-port in)
  (let ((buf (make-string http-chunked-buffer-size))
        (size 0)
        (pos 0)
        (eof (call-with-input-string "" read-char)))
    (define (fill-buffer!)
      (let* ((line (read-line in))
             (n (string->number line 16)))
        (cond
         ((not (and (integer? n) (<= 0 n http-chunked-size-limit)))
          (error "invalid chunked size line" line))
        ((zero? n))
        (else
         (set! size n)
         (if (<= n (string-length buf))
             (read-string! n buf in)
             (set! buf (read-string n in)))))))
    (make-input-port
     (lambda ()
       (if (>= pos size) (fill-buffer!))
       (cond
        ((>= pos size) eof)
        (else
         (set! pos (+ pos 1))
         (string-ref buf (- pos 1)))))
     (lambda () (or (< pos size) (char-ready? in)))
     (lambda () (close-input-port in))
     (lambda ()
       (if (>= pos size) (fill-buffer!))
       (if (>= pos size) eof (string-ref buf pos))))))

(define (http-get/raw url in-headers limit)
  (if (<= limit 0)
      (error "http-get: redirect limit reached" url)
      (let* ((uri (if (uri? url) url (string->uri url)))
             (host (and uri (uri-host uri))))
        (if (not host)
            (error "invalid url" url)
            (receive (in out)
                ((if (eq? 'https (uri-scheme uri)) ssl-connect tcp-connect)
                 host
                 (or (uri-port uri) (if (eq? 'https (uri-scheme uri)) 443 80)))
              (display "GET " out)
              (display (or (uri-path uri) "/") out)
              (display " HTTP/1.0\r\n" out)
              (display "Host: " out) (display host out) (display "\r\n" out)
              (cond
               ((not (mime-ref in-headers "user-agent"))
                (display "User-Agent: " out)
                (display http-user-agent out)
                (display "\r\n" out)))
              (for-each
               (lambda (x)
                 (display (car x) out)  (display ": " out)
                 (display (cdr x) out) (display "\r\n" out))
               in-headers)
              (display "Connection: close\r\n\r\n" out)
              (let* ((resp (http-parse-response (read-line in)))
                     (headers (mime-headers->list in))
                     (status (quotient (cadr resp) 100)))
                (case status
                  ((2)
                   (let ((enc (mime-ref headers "transfer-encoding")))
                     (cond
                      ((equal? enc "chunked")
                       (http-wrap-chunked-input-port in))
                      (else
                       in))))
                  ((3)
                   (close-input-port in)
                   (close-input-port out)
                   (let ((url2 (mime-ref headers "location")))
                     (if url2
                         (http-get/raw url2 in-headers (- limit 1))
                         (error "redirect with no location header"))))
                  (else
                   (close-input-port in)
                   (close-input-port out)
                   (error "couldn't retrieve url" url resp)))))))))

(define (http-get url . headers)
  (http-get/raw url
                (if (pair? headers) (car headers) '())
                http-redirect-limit))

(define (call-with-input-url url proc)
  (let* ((p (http-get url))
         (res (proc p)))
    (close-input-port p)
    res))

(define (with-input-from-url url thunk)
  (let ((p (http-get url)))
    (let ((res (parameterize ((current-input-port p)) (thunk))))
      (close-input-port p)
      res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; server utils

;; read and parse a request line
(define (http-parse-request . o)
  (let ((line (string-split
               (read-line (if (pair? o) (car o) (current-input-port)) 4096))))
    (cons (string->symbol (car line)) (cdr line))))

;; Parse a form body with a given URI and MIME headers (as parsed with
;; mime-headers->list).  Returns an alist of (name . value) for every
;; query or form parameter.
(define (http-parse-form uri headers . o)
  (let* ((in (if (pair? o) (car o) (current-input-port)))
         (type (mime-ref headers
                         "content-type"
                         "application/x-www-form-urlencoded")) 
         (query0 (or (uri-query (if (string? uri) (string->uri uri) uri)) '()))
         (query (if (string? query0) (uri-query->alist query0) query0)))
    (cond
     ((substring-ci=? "multipart/" type)
      (let ((mime (mime-message->sxml in headers)))
        (append
         (let lp ((ls (cddr mime))
                  (res '()))
           (cond
            ((null? ls)
             res)
            ((and (pair? (car ls))
                  (eq? 'mime (caar ls))
                  (pair? (cdar ls))
                  (pair? (cadar ls))
                  (memq (caadar ls) '(^ @)))
             (let* ((disp0 (mime-ref (cdadar ls) "content-disposition" ""))
                    (disp (mime-parse-content-type disp0))
                    (name (mime-ref disp "name")))
               (if name
                   (lp (cdr ls) (cons (cons name (caddar ls)) res))
                   (lp (cdr ls) res))))
            (else
             (lp (cdr ls) res))))
         query)))
     (else
      query))))

)
