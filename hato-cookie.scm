;;;; hato-cookie.scm -- HTTP cookie utils
;;
;; Copyright (c) 2005-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(module hato-cookie
(string->cookie cookie->string)

(import scheme chicken)

;; http://www.ietf.org/rfc/rfc2965.txt
;; http://wp.netscape.com/newsref/std/cookie_spec.html

(define-class <cookie-jar> ()
  ((cookies :init-keyword :cookies :accessor cookies-of
            :init-value (make-hash-table 'string=?))
   (file :init-keyword :file :accessor file-of :init-value #f)
   (ignore-domains :init-keyword :ignore-domains :init-value '())
   (keep-all :init-keyword :keep-all :init-value #f)
   (max-cookies :init-keyword :max-cookies :init-value #f)
   (max-cookies-per-domain :init-keyword :max-cookies-per-domain :init-value #f)
   (max-cookie-size :init-keyword :max-cookie-size :init-value #f)
   (overflow-policy :init-keyword :overflow-policy :init-value 'keep-new)
   (validator :init-keyword :validator :init-value #f)))

(define-class <cookie> ()
  ((domain :init-keyword :domain :accessor domain-of :init-value #f)
   (port :init-keyword :port :accessor port-of :init-value #f)
   (path :init-keyword :path :accessor path-of :init-value "/")
   (name :init-keyword :name :accessor name-of)
   (value :init-keyword :value :accessor value-of :init-value "")
   (expires :init-keyword :expires :accessor expires-of :init-value #f)
   (secure? :init-keyword :secure? :accessor is-secure? :init-value #f)
   (version :init-keyword :version :accessor version-of :init-value 0)))

(define-method key-of ((c <cookie>))
  (format "~A:~A:~A" (domain-of c) (path-of c) (name-of c)))

(define-method cookie-jar-add! ((cjar <cookie-jar>) (c <cookie>))
  (hash-table-put! (cookies-of cjar) (key-of c) c))

(define-method cookie-jar-remove! ((cjar <cookie-jar>) (c <cookie>))
  (hash-table-delete! (cookies-of cjar) (key-of c)))

(define *date-format1* "~a, ~d-~b-~Y ~H:~M:~S GMT") ;; hard-coded GMT
(define *date-format2* "~A, ~d-~b-~Y ~H:~M:~S GMT")

(define (parse-cookie-date str)
  (with-error-handler
      (lambda (err)
        (with-error-handler (lambda _ #f)
          (cut string->date str *date-format2*)))
    (cut string->date str *date-format1*)))

(define (string-split* str splitter . opt-len)
  (let ((res (string-split str splitter)))
    (if (pair? opt-len)
      (receive (first rest) (split-at* res (car opt-len) #t "")
        (let ((lp (last-pair first)))
          (set-car! lp (string-join (cons (car lp) rest)
                                    (if (char? splitter) (string splitter)
                                        (if (string? splitter) splitter ""))))
          first))
      res)))

(define (string->cookie str)
  (let ((c (make <cookie>))
        (attrs (map (cut string-split* <> #\= 2)
                    (string-split str #/\s*\;\s*/))))
    ;; first attr is NAME=VALUE
    (set! (name-of c) (caar attrs))
    (set! (value-of c) (cadar attrs))
    ;; handle remaning attrs
    (for-each
     (lambda (attr)
       (let ((name (string->symbol (string-downcase (car attr))))
             (value (cadr attr)))
         (case name
           ((domain path port)
            (slot-set! c name value))
           ((version)
            (slot-set! c name (string->number value)))
           ((secure)
            (set! (secure-of? c) #t))
           ((expires)
            (set! (expires-of c) (parse-cookie-date value)))
           (else
            (warn "unknown cookie attribute: ~S" name)))))
     (cdr attrs))
    c))

(define (cookie->string c)
  (with-output-to-string
    (lambda ()
      (format #t "~A=~A;" (name-of c) (value-of c))
      (when (expires-of c)
        (format #t " expires=~A\;"
                (date->string (expires-of c) *date-format1*)))
      (when (path-of c) (format #t " path=~A\;" (path-of c)))
      (when (domain-of c) (format #t " domain=~A\;" (domain-of c)))
      (when (is-secure? c) (format #t " secure")))))

;; Netscape family format, should make this separate as in Perl
(define-method load-cookies ((cjar <cookie-jar>) . opt-file)
  (with-input-from-file (get-optional opt-file (slot-ref cjar 'file))
    (lambda ()
      (port-for-each
       (lambda (line)
         (rxmatch-case line
           (#/^\s*(?:#.*)?$/ (#f) #f)
           (#/^([^\t]+)\t(TRUE|FALSE)\t([^\t]+)\t(TRUE|FALSE)\t(\d+)\t([^\t]+)\t(.*)$/i
              (#f domain bool path secure? expires name value)
            (cookie-jar-add!
             cjar
             (make <cookie>
               :domain domain
               :path path
               :secure? (string-ci=? secure? "TRUE")
               :expires (time-monotonic->date
                         (make-time time-monotonic
                                    0 (string->number expires)))
               :name name
               :value value)))
           (else
            (warn "malformed cookie line: ~S" line))))
       read-line))))

(define-method load-cookies ((file <string>))
  (let ((cjar (make <cookie-jar> :file file)))
    (when (file-is-readable? file)
      (load-cookies cjar file))
    cjar))

(define (save-cookies cjar . opt-file)
  (define (comment-header file)
    (if (file-exists? file)
      (with-input-from-file file
        (lambda ()
          (let ((res '()))
            (let loop ((line (read-line)))
              (cond ((and (string? line) (#/^\s*(?:#.*)?$/ line))
                     (push! line res)
                     (loop (read-line)))
                    (else (string-concatenate-reverse res)))))))
      "# HTTP Cookie File\n\n"))
  (let* ((file (get-optional opt-file (slot-ref cjar 'file)))
         (header (comment-header file)))
    (make-directory* (sys-dirname file))
    (with-output-to-file file
      (lambda ()
        (display header)
        (for-each
         (lambda (c)
           (format #t "~A\t~A\t~A\t~A\t~A\t~A\t~A\n"
                   (domain-of c)
                   (if (#/^\./ (domain-of c)) "TRUE" "FALSE")
                   (path-of c)
                   (if (is-secure? c) "TRUE" "FALSE")
                   (if (expires-of c)
                     (time-second (date->time-monotonic (expires-of c)))
                     2147483645) ;; pick arbitrary time in future
                   (name-of c)
                   (value-of c)))
         (hash-table-values (cookies-of cjar)))))))

(define (maybe-assoc x y . opt-eq)
  (if (pair? x)
    (assoc x y (get-optional opt-eq equal?))
    x))

(define-method extract-cookies ((cjar <cookie-jar>) (headers <pair>) . args)
  ;;(warn "extract-cookies ~S\n" headers)
  (let ((domain (get-keyword :domain args #f)))
    (for-each
     (lambda (c)
       ;; XXXX need to add count and total size checks
       (cookie-jar-add! cjar c))
     (filter
      (lambda (c)
        (let* ((cdomain (or (domain-of c) domain))
               (csize (maybe-assoc (slot-ref cjar 'max-cookie-size) cdomain)))
          (and (or (not domain) (subdomain? domain cdomain))
               (cond ((slot-ref cjar 'validator) => (cut <> c)) (else #t))
               (or (not csize)
                   (and (<= (string-size (name-of c)) csize)
                        (<= (string-size (value-of c)) csize))))))
      (map
       (lambda (h)
         (let ((c (string->cookie (cadr h))))
           (unless (domain-of c) (set! (domain-of c) domain))
           c))
       (filter (lambda (h) (#/^set-cookie2?$/i (car h)))
               headers))))))

(define-method extract-cookies ((cjar <cookie-jar>) (headers <string>) . args)
  (apply extract-cookies cjar
         (call-with-input-string headers rfc822-header->list) args))

(define-method extract-cookies ((cjar <cookie-jar>) (headers <port>) . args)
  (apply extract-cookies cjar (rfc822-header->list headers) args))

;; is URL domain a subdomain handled by the Cookie domain?
(define (subdomain? u c)
  (and (or (not u) (not c)
           (string-ci=? u c)
           (and (#/^(?:\d+\.){3}\d+$/ c)
                (and-let* ((host (sys-gethostbyaddr c AF_INET)))
                  (string-ci=? u (slot-ref host 'name))))
           (and (#/^\..+(?:\.[^.]{3,}|\.[^.]+\.[^.]{2,})$/ c)
                (or (string-suffix-ci? c u)
                    (string-ci=? u (substring c 1 (string-length c))))))
       #t))

;; is URL path a subpath handled by the Cookie path?
(define (subpath? u c)
  (and (or (not u) (not c)
           (string-prefix? c u))
       #t))

(define (date<=? d1 d2)
  (time<=? (date->time-monotonic d1)
           (date->time-monotonic d2)))

(define-method get-valid-cookies ((cjar <cookie-jar>) uri)
  ;;(warn "get-valid-cookies")
  ;;(hash-table-for-each
  ;; (cookies-of cjar)
  ;; (lambda (k v) (warn "key: ~S value: ~S" k v)))
  (let*-values
      (((scheme specific) (uri-scheme&specific uri))
       ((authority path query fragment) (uri-decompose-hierarchical specific))
       ((userinfo host port) (uri-decompose-authority (or authority ""))))
    (filter (lambda (c)
              (or (and (subdomain? host (domain-of c))
                       (subpath? path (path-of c))
                       (or (not (is-secure? c)) (eq? scheme 'https))
                       (or (not (expires-of c))
                           (date<=? (current-date) (expires-of c)))
                       )
                  (begin
                    ;;(warn "not sending cookie: ~S (~S)\n" c (name-of c))
                    #f)))
            (hash-table-values (cookies-of cjar)))))

(define-method send-cookies ((cjar <cookie-jar>) uri)
  (let ((valid (get-valid-cookies cjar uri)))
    (when (pair? valid)
      (display "Cookie:")
      (for-each
       (lambda (c) (format #t " ~A=~A;" (name-of c) (value-of c)))
       valid)
      (display "\r\n"))))

)
