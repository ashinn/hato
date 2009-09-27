;; domain-keys.scm -- Yahoo DomainKeys
;;
;; Copyright (c) 2005-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; Procedure: domain-key-verify mail-text
;;
;; Returns #f if Domain-Key was both present and invalid, otherwise one
;; of the following symbols is returned indicating success:
;;
;;   valid                - DomainKey was found and verified
;;   invalid-but-testing  - DomainKey was found and invalid, but in testing mode
;;   invalid-public-key   - the public key was invalid
;;   no-public-key        - the public key was empty
;;   unknown-key-type     - the KeyType was unknown (i.e. not RSA)
;;   invalid-dns-txt      - the DNS TXT entry was invalid
;;   no-dns-txt           - there was no DNS TXT entry
;;   invalid-domain-key   - the DomainKey was found but not in a valid format
;;   no-domain-key        - there was no DomainKey
;;
;; In practice the result will likely never be #f - no one would bother
;; to insert a Domain-Key unless it were valid.  The trick is to check
;; and require valid keys from domains which define them.

;; Procedure: domain-key-signature mail-text key-file
;;                [domain selector headers algorithm normalizer query]
;;
;; Returns the string value of the DomainKey-Signature header for the
;; given MAIL-TEXT signed by the private key in KEY-FILE.  DOMAIN is the
;; domain to sign for.  SELECTOR is the selector, if any, to be combined
;; with the DOMAIN.  HEADERS should be a list of strings specifying
;; which headers to include in the signature, and defaults to all
;; current headers.  ALGORITHM is the openssl algorithm to use and
;; defaults to 'rsa-sha1.  NORMALIZER is the normalization to perform on
;; the message, defaulting to 'nofws.  QUERY is the method by which the
;; client should obtain the public key, and defaults to the only current
;; available method 'dns.

;; Procedure: domain-key-sign mail-text key-file
;;                [domain selector headers algorithm normalizer query]
;;
;; Convenience routine, returns a complete new MAIL-TEXT string, signed
;; as with the same arguments to DOMAIN-KEY-SIGNATURE.

(require-library dns hato-base64 hato-mime)

(module domain-keys
  (domain-key-verify domain-key-sign domain-key-signature
   dkey-normalize-simple dkey-normalize-nofws)

(import scheme chicken extras ports data-structures regex posix dns hato-base64 hato-mime)

(define (string-strip-whitespace str)
  (string-translate str " \t"))

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

;; writes str to a temp file and returns the name of the file
(define (string->temp-file str . o)
  (let ((name (if (pair? o) (car o) "temp")))
    (receive (fd path)
        (file-mkstemp (string-append "/tmp/dkey-" name ".XXXXXX"))
      (let ((port (open-output-file* fd)))
        (display str port)
        (close-output-port port)
        path))))

(define (dkey-normalize msg domain header-proc value-proc body . o)
  (define (any pred ls)
    (let lp ((ls ls)) (and (pair? ls) (or (pred (car ls)) (lp (cdr ls))))))
  (let ((header-pred
         (if (and (pair? o) (car o))
           (let ((headers (if (pair? (car o)) (car o) (string-split (car o) ":"))))
             (lambda (h) (any (cut string-ci=? h <>) headers)))
           (lambda _ #t))))
    (with-input-from-port (if (string? msg) (open-input-string msg) msg)
      (lambda ()
        (with-output-to-string
          (lambda ()
            (mime-header-fold
             (lambda (header value acc)
               (when (and acc (header-pred header))
                 (display (header-proc header)) (display ":")
                 (display (value-proc value)) (display "\r\n"))
               (or acc
                   (and-let* ((t1 (string-ci=? "domainkey-signature" header))
                              (dsig (mime-parse-content-type value))
                              (ddom (mime-ref dsig "d")))
                     (string-ci=? domain ddom))))
             #f)
            (body)))))))

(define (dkey-normalize-simple msg domain . o)
  (apply dkey-normalize msg domain identity identity
         (lambda () (display "\r\n") (display (read-string)))
         o))

(define (dkey-normalize-nofws msg domain . o)
  (apply dkey-normalize msg domain identity string-strip-whitespace
         (lambda ()
           (let lp ((trailing '("")))
             (let ((line (read-line)))
               (cond
                 ((eof-object? line)
                  #f)
                 ((string=? line "")
                  (lp (cons line trailing)))
                 (else
                  (for-each
                   (lambda (x) (display x) (display "\r\n"))
                   (reverse trailing))
                  (display (string-strip-whitespace line))
                  (display "\r\n")
                  (lp '()))))))
         o))

(define (normalizer->procedure x)
  (cond
    ((procedure? x) x)
    ((string? x) (normalizer->procedure (string->symbol x)))
    (else
     (case x
       ((nofws) dkey-normalize-nofws)
       (else dkey-normalize-simple)))))

(define (domain-key-verify mail-string . o)
  (define verify-command-format
    "openssl dgst -verify ~A -~A -signature ~A ~A")
  (let* ((headers (if (pair? o)
                    (car o)
                    (with-input-from-string mail-string mime-headers->list)))
         (dkey-header (mime-ref headers "domainkey-signature")))
    (if (not dkey-header)
      'no-domain-key
      (let* ((dkey (mime-parse-content-type dkey-header))
             (domain (mime-ref dkey "d"))
             (signature (string-strip-whitespace (mime-ref dkey "b"))))
        (if (not (and domain signature))
          'invalid-domain-key
          (let* ((selector (mime-ref dkey "s"))
                 (dkey-domain
                  (if selector
                    (string-append selector "._domainkey." domain)
                    (string-append "_domainkey." domain)))
                 (txt (dns-text dkey-domain)))
            (if (or (not txt) (string=? txt ""))
              'no-dns-txt
              (let ((txt-object (condition-case
                                 (with-input-from-string txt read)
                                 (e () #f))))
                (if (or (not (string? txt-object)) (string=? txt ""))
                  'invalid-dns-txt
                  (let* ((txt-keys (mime-parse-content-type txt-object))
                         (key-type (mime-ref txt-keys "k" "rsa"))
                         (testing? (string-ci=? (mime-ref txt-keys "t" "n") "y"))
                         (public
                          (string-strip-whitespace (mime-ref txt-keys "p" ""))))
                    (if (not (string-ci=? key-type "rsa"))
                      'unknown-key-type
                      (if (string=? public "")
                        'no-public-key
                        (let* ((temp-text
                                (string->temp-file
                                 ((normalizer->procedure
                                   (mime-ref dkey "c" 'nofws))
                                  mail-string domain (mime-ref dkey "h"))))
                               (temp-public
                                (string->temp-file
                                 (string-append
                                  "-----BEGIN PUBLIC KEY-----\n"
                                  (string-intersperse (string-chop public 64) "\n")
                                  "\n-----END PUBLIC KEY-----\n")))
                               (temp-sig
                                (string->temp-file
                                 (base64-decode-string signature))))
                          (let* ((cmd (sprintf verify-command-format
                                               (shell-escape temp-public)
                                               (ssl-algorithm-name
                                                (mime-ref dkey "a" "rsa-sha1"))
                                               (shell-escape temp-sig)
                                               (shell-escape temp-text)))
                                 (status (system cmd)))
                            ;; keep temp files from failures when debugging
                            (when (or (cond-expand (debug #t) (else #f))
                                      (zero? status))
                              (delete-file temp-text)
                              (delete-file temp-public)
                              (delete-file temp-sig))
                            (cond
                              ((zero? status) 'valid)
                              (testing?       'invalid-but-testing)
                              (else           #f))))))))))))))))

(define (ssl-algorithm-name x)
  (if (string? x)
    (ssl-algorithm-name (string->symbol x))
    (case x
      ((rsa-sha1) 'sha1)
      (else x))))

;; sign a string with a given private key file and return the result as
;; a base64 encoded string
(define (ssl-sign str private-key-file . o)
  (let* ((algorithm (if (pair? o) (car o) 'sha1))
         (cmd (sprintf "openssl dgst -sign ~A -~A"
                       (shell-escape private-key-file)
                       (ssl-algorithm-name algorithm))))
    (receive (in out pid) (process cmd)
      (display str out)
      (flush-output out)
      (close-output-port out)
      (let ((res
             (base64-encode-string
              (read-string #f in))))
        (process-wait pid)
        res))))

(define (domain-key-signature mail-string private-key-file . o)
  (let-optionals* o ((domain #f)
                     (selector #f)
                     (headers #f)
                     (algorithm 'rsa-sha1)
                     (normalizer 'nofws)
                     (query 'dns))
    (let* ((headers (if (pair? headers)
                      headers
                      (map car (mime-headers->list mail-string))))
           (str ((normalizer->procedure normalizer)
                 mail-string (or domain "") headers))
           (sig (ssl-sign str private-key-file algorithm)))
      (sprintf "a=~A; q=~A; c=~A; ~A~Ah=~A; b=~A"
               algorithm
               query
               normalizer
               (if selector (sprintf "c=~A; " selector) "")
               (if domain (sprintf "d=~A; " domain) "")
               (string-intersperse headers ":")
               sig))))

(define (domain-key-sign mail-string private-key-file . o)
  (string-append
   "DomainKey-Signature: "
   (apply domain-key-signature mail-string private-key-file o)
   "\n"
   mail-string))

)
