;;;; hato-smtp.scm -- simple SMTP module
;;
;; Copyright (c) 2005-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; Easy mail interface.
;;
;; Example:
;;
;; (send-mail From:    "Dr. Watson <guest@grimpen.moor>"
;;            To:      "Sherlock Homes <not-really@221B-baker.street>"
;;            Subject: "First Report"
;;            Charset: "ISO-8859-1"
;;            Body:    "Moor is gloomy. Heard strange noise, attached."
;;            Attachments: '((File: "howl.ogg")))
;;
;; This tries very hard to do The Right Thing.
;;
;; More specifically, the body and headers are all properly MIME-encoded
;; for the given Charset (which may alternately be specified in
;; Content-Type), choosing the whichever of base64 and quoted-printable
;; would encode with the smallest size.  Source strings are all assumed
;; to be UTF-8 and charset encoded as needed.
;;
;; Attachments are arbitrarily nested lists of the same keyword
;; parameters as in the main send-mail parameters, and are sent as
;; multipart MIME encodings (i.e. any valid parameter list to send-mail
;; could also be used in an Attachments: list).  Attachments inherit,
;; but may override, their parents' Charset settings.
;;
;; Most standard mail headers are recognized as keyword arguments,
;; always titlecased (e.g. Cc:, Message-Id:, Mime-Version:) for a
;; friendly mail-like interface.  Additional headers may be specified in
;; the Headers: keyword, for example
;;
;;   Headers: '(X-Foo: "bar" X-Baz: "qux")
;;
;; The parameters To:, Cc: and Bcc: may also be lists of addresses which
;; will be properly comma-delimited.
;;
;; You can tell send-mail *not* to perform character encoding
;; conversions by passing the Charconv: #f keyword argument, and
;; likewise disable automatic MIME encoding of message bodies by passing
;; Encode: #f, in which case you are responsible for ensuring the
;; literal text matches with the given Content-type and
;; Content-Transfer-Encoding.
;;
;; If you just want to send a raw message without any processing at all,
;; you can pass an input-port to the Source: parameter
;;
;;   (send-mail To: addresses Source: (open-input-file "message.txt"))
;;
;; By default send-mail connects to the local smtp port, however you can
;; override this with the Host: parameter to use a particular server.
;; Alternately, if you specify Host: #f then send-mail will look up the
;; appropriate DNS entries for all recipients and send to them in turn.
;;
;; send-mail returns the list of recipients who couldn't be delivered to
;; (i.e. a null list when there are no errors).

(require-library autoload srfi-1 dns
                 hato-archive hato-mime hato-base64 quoted-printable)

(module hato-smtp
 (send-mail mime-encode-header mime-choose-encoding
  smtp-write-message smtp-write-headers smtp-write-mime-part
  smtp-generate-msgid smtp-generate-boundary
  smtp-open smtp-close smtp-quit smtp-starttls smtp-helo
  smtp-mail smtp-rcpt smtp-data smtp-noop smtp-reset
  smtp-verify smtp-expand smtp-mail-from smtp-recipient
  smtp-status smtp-message smtp-help smtp?
  domain-part local-part)

(import scheme chicken extras utils data-structures ports files posix
        srfi-1 srfi-69 tcp dns
        autoload hato-archive hato-mime hato-base64 quoted-printable)

(autoload charconv ces-convert)
(autoload ssl ssl-wrap unwrap-tcp-ports)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant *smtp-mime-version* "1.0")
(define-constant *smtp-default-port* 25)
(define-constant *default-max-col* 76)

(define-record-type <smtp>
  (%make-smtp in out status message debug?)
  smtp?
  (in smtp-in set-smtp-in!)
  (out smtp-out set-smtp-out!)
  (status smtp-status set-smtp-status!)
  (message smtp-message set-smtp-message!)
  (debug? smtp-debug?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

;; use network newline convention regardless of the client platform
(define (pr str . o)
  (let ((p (or (and (pair? o) (car o)) (current-output-port))))
    (display str p)
    (display "\r\n" p)))

(define (copy-data-port in out . o)
  (let ((nl (if (pair? o) (car o) "\r\n")))
    (let lp ()
      (let ((str (read-line in)))
        (cond
         ((not (eof-object? str))
          (if (and (> (string-length str) 0) (eqv? #\. (string-ref str 0)))
              (write-char #\. out))
          (display str out)
          (display nl out)
          (lp)))))))

(define (smtp-write-data str out . o)
  (call-with-input-string str (cut apply copy-data-port <> out o)))

(define (current-seconds-string) ; trim the .0
  (let* ((now (number->string (current-seconds)))
         (now-len (string-length now)))
    (if (eqv? #\. (string-ref now (- now-len 2)))
        (substring now 0 (- now-len 2))
        now)))

;; ARGH!!! remove the stupid newline
(define (current-date-string)
  (let* ((s (seconds->string (current-seconds)))
         (len (- (string-length s) 1)))
    (if (eqv? #\newline (string-ref s len))
        (substring s 0 len)
        s)))

(define (get-user-name)
  (car (user-information (current-user-id))))

(define (domain-part addr)
  (let lp ((i (- (string-length addr) 1)))
    (and (>= i 0)
         (if (eqv? #\@ (string-ref addr i))
             (substring addr (+ i 1))
             (lp (- i 1))))))

(define (local-part addr)
  (let lp ((i (- (string-length addr) 1)))
    (if (< i 0)
        addr
        (if (eqv? #\@ (string-ref addr i))
            (substring addr 0 i)
            (lp (- i 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parsing smtp responses

(define (smtp-get-response smtp)
  (let lp ((line (read-line (smtp-in smtp))) (msg '()))
    (cond
     ((eof-object? line)
      (smtp-close smtp)
      #f)
     (else
      (if (smtp-debug? smtp)
          (fprintf (current-error-port) "smtp-resp: ~A\n" line))
      (let ((len (string-length line)))
        (if (and (>= len 4) (eqv? #\- (string-ref line 3)))
            (lp (read-line (smtp-in smtp)) (cons (substring line 4) msg))
            (and-let* (((>= len 3))
                       (status (string->number (substring line 0 3)))
                       (str (if (= len 3) "" (substring line 4))))
              (set-smtp-status! smtp status)
              (set-smtp-message! smtp (string-intersperse (cons str msg) " "))
              (< status 400))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; connecting

(define (smtp-open host . o)
  (let-optionals* o ((port #f)
                     (from-host (get-host-name))
                     (debug? #f))
    (let ((port (or port *smtp-default-port*)))
      (receive (in out)
          (condition-case
              (tcp-connect host port)
            (exn ()
                 (if debug?
                     (fprintf (current-error-port)
                              "smtp: couldn't connect to ~S:~S\n"
                              host port))
                 (values #f #f)))
        (and in out
             (let ((smtp (%make-smtp in out 200 "" #f)))
               (if (and (smtp-get-response smtp)
                        (smtp-helo smtp from-host))
                   smtp
                   (begin
                     (smtp-close smtp)
                     #f))))))))

(define (smtp-close smtp)
  (close-input-port (smtp-in smtp))
  (close-output-port (smtp-out smtp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic commands

;; catch i/o errors
(define (make-smtp-command name)
  (lambda (smtp . o)
    (let ((out (smtp-out smtp)))
      (condition-case
          (begin
            (if (smtp-debug? smtp)
                (fprintf (current-error-port) "smtp-send: ~S\n" o))
            (display name out)
            (cond
             ((pair? o)
              (write-char #\space out)
              (display (car o) out)))
            (pr " " out)
            (smtp-get-response smtp))
        (exn ()
             (set-smtp-status! smtp 554) ; transaction failed
             (set-smtp-message! smtp #f)
             #f)))))

(define smtp-helo     (make-smtp-command 'HELO))
(define smtp-mail     (make-smtp-command 'MAIL))
(define smtp-rcpt     (make-smtp-command 'RCPT))
(define smtp-data     (make-smtp-command 'DATA))
(define smtp-reset    (make-smtp-command 'RSET))
(define smtp-verify   (make-smtp-command 'VRFY))
(define smtp-expand   (make-smtp-command 'EXPN))
(define smtp-help     (make-smtp-command 'HELP))
(define smtp-quit     (make-smtp-command 'QUIT))
(define smtp-noop     (make-smtp-command 'NOOP))

;; STARTTLS is an abomination.  Instead of just wrapping the entire
;; protocol in a clean layer like normal SSL, you start out in an
;; unencrypted protocol and then request to switch to SSL,
;; complicating every protocol that uses this and breaking the nice
;; `ssl-connect' abstraction by forcing us to put protocol-specific
;; code here.
(define (smtp-starttls smtp)
  (and ((make-smtp-command 'STARTTLS) smtp)
       (let ((fd (unwrap-tcp-ports (smtp-in smtp) (smtp-out smtp))))
         (receive (in out) (ssl-wrap fd)
           (set-smtp-in! smtp in)
           (set-smtp-out! smtp out)
           #t))))

(define (smtp-mail-from smtp address)
  (pr (string-append "MAIL FROM:<" address ">") (smtp-out smtp))
  (smtp-get-response smtp))

(define (smtp-recipient smtp address)
  (pr (string-append "RCPT TO:<" address ">") (smtp-out smtp))
  (smtp-get-response smtp))

(define (smtp-generate-boundary)
  (string-append "------Multipart_" (number->string (current-process-id))
                 "_" (symbol->string (gensym))
                 "." (current-seconds-string)))

(define (smtp-generate-msgid)
  (string-append "<" (current-seconds-string)
                 "." (number->string (current-process-id))
                 "." (symbol->string (gensym))
                 "@" (get-host-name) ">"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; high-level interface

(define (build-mx-table rcpt-ls localhost . o)
  (let ((domain-mxs (if (pair? o) (car o) (make-hash-table string=?)))
        (hosts (make-hash-table string=?)))
    (for-each
     (lambda (addr)
       (let* ((domain (or (domain-part addr) localhost))
              (mx (or (hash-table-ref/default domain-mxs domain #f)
                      (let ((res (dns-mx domain)))
                        (hash-table-set! domain-mxs domain res)
                        res))))
         (if (pair? mx)
             (hash-table-set!
              hosts
              (cadar mx)
              (cons addr (hash-table-ref/default hosts (cadar mx) '()))))))
     rcpt-ls)
    hosts))

;; convenience routine to send a single message
(define (send-mail
         #!rest keys
         #!key
         (Host "localhost")
         (Port *smtp-default-port*)
         (User (get-user-name))
         (Password #f)
         (Localhost (get-host-name))
         (From (string-append User "@" Localhost))
         (From-Base (car (parse-mail-address From)))
         (From-Addr From-Base)
         (To '())
         (Cc '())
         (Bcc '())
         (Recipients (map car (map parse-mail-address (flatten To Cc Bcc))))
         (Source #f)
         (StartTLS? #f)
         (Auth-Method #f)
         (No-Sendmail #f))
  (define (send-output out)
    (cond
     ((input-port? Source) (copy-data-port Source out))
     ((procedure? Source)  (Source out))
     ((string? Source)     (smtp-write-data Source out))
     (else (apply smtp-write-message out keys)))
    (display "\r\n.\r\n" out))
  (define (send host rcpt in)
    (and-let* ((smtp (smtp-open host Port Localhost))
               (out (smtp-out smtp)))
      ;; optionally negotiate TLS and authenticate
      (if StartTLS?
          (smtp-starttls smtp))
      (if (or Password Auth-Method)
          #f)
      ;; mail to all recipients
      (let ((res 
             (and (smtp-mail-from smtp From-Addr)
                  (every (cut smtp-recipient smtp <>) rcpt)
                  (smtp-data smtp)
                  (begin
                    (send-output out)
                    (smtp-get-response smtp)))))
        (smtp-quit smtp)
        (smtp-close smtp)
        res)))
  (if (string? Host)
      ;; local or smarthost
      (if (send Host Recipients Source)
          '()
          ;; if relaying to the localhost failed, optionally fall back
          ;; to the sendmail executable
          (if (and (string-ci=? Host "localhost") (not No-Sendmail))
              (receive (in out pid) (process "sendmail" Recipients)
                (send-output out)
                (close-output-port out)
                (receive (pid ok? status) (process-wait pid)
                  (if (and ok? (zero? status)) '() Recipients)))
              Recipients))
      ;; buffer message to send to multiple smtp hosts
      (let ((source (or Source (call-with-output-string send-output)))
            (hosts (build-mx-table Recipients Localhost)))
        ;; connect to each host and deliver the message in turn to all
        ;; recipients on that host
        (let ((failures '())) ; XXXX TODO, try secondary MX hosts
          (hash-table-walk
           hosts
           (lambda (host ls)
             (unless (send host ls source)
               (set! failures (append ls failures)))))
          failures))))

(define (smtp-write-message port . keys)
  (apply smtp-write-headers port keys)
  (apply smtp-write-mime-part port keys))

(define (multi-part-type? str)
  (and str (substring-index-ci "multipart/" str) #t))

(define (ascii-string? str)
  (let lp ((i (- (string-length str) 1)))
    (or (< i 0)
        (and (< (char->integer (string-ref str i)) 127)
             (lp (- i 1))))))

(define (mime-choose-encoding str)
  (let lp ((i (- (string-length str) 1)) (ascii 0) (qp 0) (hi 0))
    (if (< i 0)
        (cond
         ((zero? hi) "7-bit")
         ((<= (+ ascii (* 3 (+ qp hi)))
              (/ (* 4 (+ ascii qp hi)) 3))
          "quoted-printable")
         (else "base64"))
        (let ((c (char->integer (string-ref str i))))
          (cond
           ((>= c 128)
            (lp (- i 1) ascii qp (+ hi 1)))
           ((and (<= 33 c 126) (not (memq c '(61 63 95))))
            (lp (- i 1) (+ ascii 1) qp hi))
           (else
            (lp (- i 1) (+ ascii 1) (+ qp 1) hi)))))))

(define (mime-choose-encoder str)
  (let ((enc (mime-choose-encoding str)))
    (cond
     ((equal? "quoted-printable" enc) quoted-printable-encode-header)
     ((equal? "base64" enc) base64-encode-header)
     (else identity))))

(define (mime-wrap-header-value val . o) ; try to wrap after semi-colons
  (let* ((width (if (pair? o) (car o) 0))
         (nl (if (and (pair? o) (pair? (cdr o))) (cadr o) "\r\n"))
         (sep (string-append nl "\t")))
    (let lp ((i 0)
             (max-len (- *default-max-col* width))
             (len (string-length val))
             (res '()))
      (cond
       ((< len max-len)
        (string-intersperse (reverse (cons (substring val i) res)) sep))
       ((substring-index ";" val i)
        => (lambda (i2)
             (lp (+ i2 1)
                 *default-max-col*
                 (- len (- (+ i2 1) i))
                 (cons (mime-wrap-header-value (substring val i (+ i2 1))
                                               width
                                               nl)
                       res))))
       (else
        (let ((i2 (+ i max-len)))
          (lp i2 *default-max-col* (- len max-len)
              (cons (substring val i i2) res))))))))

(define (mime-encode-ascii-header name val . o)
  (let* ((nl (if (pair? o) (car o) "\r\n"))
         (name (if (keyword? name) (keyword->string name) name))
         (len1 (+ 2 (string-length name)))
         (len2 (string-length val)))
    (cond
     ((<= (+ len1 len2) *default-max-col*)
      (string-append name ": " val))
     ((>= len1 *default-max-col*) ; the header name itself is too long
      (string-append name ":" nl "\t" (mime-wrap-header-value val 0 nl)))
     (else
      (string-append name ": " (mime-wrap-header-value val len1 nl))))))

(define (mime-encode-header name val charset . o)
  (let ((nl (if (pair? o) (car o) "\r\n"))
        (name (if (keyword? name) (keyword->string name) name)))
    (if (ascii-string? val)
        (mime-encode-ascii-header name val nl)
        (let* ((str (if charset (ces-convert val "UTF-8" charset) val))
               (encode (mime-choose-encoder str)))
          (string-append name ": " (encode val (+ 2 (string-length name))
                                           0 *default-max-col* nl))
          ))))

(define *default-smtp-headers*
  `(Return-Path: Envelope-To: Delivery-Date: Received:
    (Message-Id: ,smtp-generate-msgid)
    (From: ,(lambda () (string-append (get-user-name) "@" (get-host-name))))
    Reply-To: To: Cc: (Subject: ,(lambda () ""))
    (Date: ,current-date-string)
    X-Mailer: X-Accept-Language: User-Agent: Organization: X-Face:
    (Mime-Version: ,(lambda () *smtp-mime-version*)) X-Loop: X-Priority:
    ))

(define (smtp-write-headers
         port
         #!rest keys
         #!key
         (Content-Type #f)
         (Charset (and Content-Type
                       (mime-ref (mime-parse-content-type Content-Type)
                                 "charset")))
         (Charconv #t)
         (Headers '())
         (Newline "\r\n"))
  (let ((cset (and Charconv Charset)))
    (for-each ; standard headers
     (lambda (x)
       (let* ((name (if (pair? x) (car x) x))
              (val1 (get-keyword name keys
                                 (lambda () (and (pair? x) ((cadr x))))))
              (val (if (pair? val1) (string-intersperse val1 ", ") val1)))
         (cond
          (val
           (display (mime-encode-header name val cset Newline) port)
           (display Newline port)))))
     *default-smtp-headers*)
    (let lp ((ls Headers)) ; custom headers
      (when (pair? ls)
        (display (mime-encode-header (car ls) (cadr ls) cset Newline) port)
        (display Newline port)
        (lp (cddr ls))))))

;; recursively write mime-parts
(define (smtp-write-mime-part
         port
         #!key
         (File #f)
         (Body #f)
         (Content-Type #f)
         (Boundary (smtp-generate-boundary))
         (Charset #f)
         (Source-Charset "UTF-8")
         (Charconv #t)
         (Content-Transfer-Encoding #f)
         (Content-Disposition #f)
         (Encode #t)
         (Attachments '())
         (Newline "\r\n"))
  (let* ((ctype (if (pair? Attachments)
                    (if (multi-part-type? Content-Type)
                        Content-Type
                        (string-append "multipart/mixed; boundary=\""
                                       Boundary "\""))
                    (or Content-Type "text/plain")))
         (ctype-ls (mime-parse-content-type ctype))
         (cset (and Charconv (or Charset (mime-ref (cdr ctype-ls) "charset"))))
         (str (or (and Body
                       (if (and Charconv cset (not (ascii-string? Body)))
                           (ces-convert Body Source-Charset cset)
                           Body))
                  (and File (with-input-from-file File read-string))
                  ""))
         (cenc (and Encode (or Content-Transfer-Encoding
                               (and (not (ascii-string? str))
                                    (mime-choose-encoding str)))))
         (ctype2 (if (and Charset (not File) (not (pair? Attachments))
                          (not (substring-index-ci "charset=" ctype)))
                     (string-append ctype "; charset=\"" Charset "\"")
                     ctype))
         (ctype3 (if (and File (not (pair? Attachments))
                          (not (substring-index-ci "filename=" ctype)))
                     (string-append ctype2 "; filename=\""
                                    (pathname-strip-directory File)
                                    "\"")
                     ctype2)))
    ;; write the type and optionally transfer-encoding headers
    (display (mime-encode-header "Content-Type" ctype3 cset) port)
    (display Newline port)
    (cond
     ((or Content-Transfer-Encoding cenc)
      (display (mime-encode-header "Content-Transfer-Encoding"
                              (or Content-Transfer-Encoding cenc)
                              cset)
          port)
      (display Newline port)))
    (cond
     (Content-Disposition
      (display (mime-encode-header "Content-Disposition"
                                   Content-Disposition
                                   cset)
          port)
      (display Newline port)))
    (display Newline port)
    ;; write the attachments
    (if (pair? Attachments)
        (let ((Attachments
               (cons (list
                      Body: Body
                      File: File
                      Content-Type: Content-Type
                      Content-Transfer-Encoding: Content-Transfer-Encoding)
                     Attachments)))
          (for-each
           (lambda (keys)
             (display "--" port)
             (display Boundary port)
             (display Newline port)
             (display Newline port)
             (apply smtp-write-mime-part port
                    (append keys (list Charset: Charset Charconv: Charconv)))
             (display Newline port))
           Attachments)
          (display "--" port)
          (display Boundary port)
          (display Newline port))
        ;; or a single body
        (if (not (string=? str ""))
            (with-output-to-port port
              (lambda ()
                (cond
                 ((equal? cenc "quoted-printable")
                  (quoted-printable-encode str))
                 ((equal? cenc "base64") ; XXXX slow
                  (display
                   (string-intersperse
                    (string-chop (base64-encode-string str) *default-max-col*)
                    Newline)))
                 (else (smtp-write-data str port Newline)))
                (display Newline port)))))))

)
