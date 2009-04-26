#! /usr/local/bin/csi -script
;; hato-mta.scm -- a mail server in Scheme
;;
;; Copyright (c) 2005-2008 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use
 extras regex posix srfi-1 srfi-13 srfi-18 srfi-69 tcp tcp-server lolevel
 domain-keys lru-cache user-env hato-daemon hato-config hato-db
 hato-prob hato-smtp hato-filter hato-spf hato-rfc3028 dns utils
 hato-archive hato-mime sandbox
 )

(include "let-args.scm")
(include "hato-log.scm")

(define undef (if #f #f))
(define-inline (defined? x) (not (eq? x undef)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exception utils

(define exception-message
  (let ((get-msg (condition-property-accessor 'exn 'message))
        (get-args (condition-property-accessor 'exn 'arguments)))
    (lambda (exn) (sprintf "~A ~S" (get-msg exn) (get-args exn)))))

(define (save-current-io-ports thunk)
  (let* ((in (current-input-port))
         (out (current-output-port))
         (err (current-error-port))
         (res (thunk)))
    (current-input-port in)
    (current-output-port out)
    (current-error-port err)
    res))

;; CONDITION-CASE is not required to save and restore current I/O ports,
;; or in fact any aspect of the dynamic environment apart from the
;; CURRENT-EXCEPTION-HANDLER itself.  For convenience of many procedures
;; such as HATO-REPL, and out of necessity for procedures like user
;; filters, we want to bind the current I/O ports accordingly.  However,
;; if an exception is raised it is crucial to restore the original
;; ports, or the server may become unusable.  In these cases we use a
;; variant CONDITION-CASE* which does always restore the ports.  This
;; assumes the exception is caught and not passed to the next handler.

(define-macro (condition-case* . x)
  `(save-current-io-ports
    (lambda () (condition-case ,@x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I/O Utils

(define (pr str . o)
  (let ((p (or (and (pair? o) (car o)) (current-output-port))))
    (display str p)
    (display "\r\n" p)))

(define (system-string str)
  (with-input-from-pipe str read-string))

(define (system-line str)
  (with-input-from-pipe str read-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thread Utils

(define (background thunk)
  (if hato-test-mode?
    (thunk)
    (thread-start! (make-thread thunk))))

(define (background-periodic seconds thunk)
  (background
   (lambda ()
     (let lp ()
       (thread-sleep! seconds)
       (thunk)
       (lp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Utils

(define (path-as-directory dir)
  (make-pathname dir ""))

(define (get-user-name . o)
  (car (user-information (if (pair? o) (car o) (current-user-id)))))

(define (get-user-home user)
  (if hato-virtual-users?
      (string-append hato-virtual-home-directory
                     (if (number? user) (get-user-name user) user))
      (caddr (cdddr (user-information user)))))

;; pwd is always ~/Mail/
(define (expand-user-dir user dir)
  (cond
    ((eqv? #\~ (string-ref dir 0))
     (let ((slash (string-index dir #\/)))
       (cond
        ((eqv? 1 slash)
         (string-append (get-user-home user) (substring dir 1)))
        (slash
         (string-append (get-user-home (substring dir 1 slash))
                        (substring dir slash)))
        (else
         (string-append (get-user-home (substring dir 1)) "/")))))
    ((and (not (string-null? dir)) (eqv? #\/ (string-ref dir 0)))
      dir)
    (else
     (string-append (get-user-home user) "/Mail/" dir))))

(define (find-file-in-paths name ls)
  (let lp ((ls ls))
    (and (pair? ls)
         (let ((f (string-append (car ls) "/" name)))
           (if (file-exists? f) f (lp (cdr ls)))))))

(define (create-directory* f)
  (let* ((end (- (string-length f) 1))
         (f/ (if (eqv? #\/ (string-ref f end)) (substring f 0 end) f)))
    (or (file-exists? f/)
        (condition-case (create-directory f/)
          (e (exn file)
             (let ((dir (pathname-directory f/)))
               (and (string? dir)
                    (not (string=? dir f/))
                    (create-directory* dir)
                    (create-directory f/))))))))

(define (file-suid? file)
  (not (zero? (bitwise-and perm/isuid (file-permissions file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; /etc/aliases support

(define load-alias-file
  (let ((alias-rx (regexp "^([^ \t:]+)[ \t]*:?[ \t]*([^#]+)")))
    (lambda (file)
      (and (file-exists? file)
           (let ((tab (make-hash-table)))
             (call-with-input-file file
               (lambda (p)
                 (let lp ()
                   (let ((line (read-line p)))
                     (unless (eof-object? line)
                       (and-let* ((m (string-match alias-rx line)))
                         (hash-table-set! tab
                                          (string-downcase (cadr m))
                                          (string-split (caddr m) " \t,")))
                       (lp))))))
             ;;(log-debug "aliases: ~S" (hash-table->alist tab))
             tab)))))

(define (hato-local-alias-domain? domain)
  (string-ci=? domain "localhost"))

(define (lookup-alias table key domain)
  (and (hato-local-domain? domain)
       table
       (let* ((key (string-downcase key))
              (res (hash-table-ref/default table key #f)))
         (if (and (pair? res) (null? (cdr res)) (string-ci=? (car res) key))
               #f  ; let this fall through to normal user processing
               res))))

;; lookup in the order used for exim/postfix virtual files:
(define (lookup-virtual-alias table key domain)
  (and (hato-local-domain? domain)
       table
       (let* ((key (string-downcase key))
              (domain (string-downcase domain))
              (full (string-append key "@" domain)))
         (define (lookup str)
           (hash-table-ref/default table str #f))
         (let ((res
                (or
                 ;; first try a full address match
                 (lookup full)
                 ;; ... then try just the username
                 (lookup key)
                 ;; ... then try a final @domain match
                 (lookup (string-append "@" domain)))))
           (if (and (pair? res) (null? (cdr res))
                    (or (string-ci=? (car res) key)
                        (string-ci=? (car res) full)))
               #f  ; let this fall through to normal user processing
               res)))))

(define (sub-domains domain)
  (let ((i (string-index domain #\.)))
    (if i
      (let ((j (string-index domain #\. (+ i 1))))
        (if j
          (cons domain (sub-domains (substring domain (+ i 1))))
          (list domain)))
      (list domain))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Settings

(define hato-version 0)
(define hato-revision 1)
(define hato-is-root? (zero? (current-user-id)))
(define hato-debug? #f)
(define hato-test-mode? #f)
(define hato-no-fork? #f)
(define hato-port #f)
(define hato-user-id #f)
(define hato-group-id #f)
(define hato-load-filter-timeout 60)
(define hato-run-filter-timeout 120)
(define hato-connection-timeout 300)
(define hato-disconnect-on-error? #f)
(define hato-verify-from-domain-existence? #f)
(define hato-verify-from-domain-matches-address? #f)
(define hato-max-received-count #f)
(define hato-max-requests #f)
(define hato-queue-delay #f)
(define hato-max-resend-attempts #f)
(define hato-retry-time-list #f)
(define hato-jump-the-gun-limit #f)
(define hato-host-name #f)
(define hato-relay-domains #f)
(define hato-local-ip-addresses #f)
(define hato-local-domains #f)
(define hato-virtual-users? #f)
(define hato-virtual-users #f)
(define hato-filter-cache-limit #f)
(define hato-base-directory #f)
(define hato-spool-directory #f)
(define hato-queue-directory #f)
(define hato-virtual-home-directory #f)
(define hato-system-filter-file #f)
(define hato-log-directory #f)
(define hato-run-directory #f)
(define hato-pid-file #f)
(define hato-time-zone #f)
(define hato-domain-keys-policy-alist
  '(("gmail.com" . (#t . all))))
(define hato-verify-domain-keys? #f)
(define hato-verify-spf? #f)
(define hato-arguments '())

;; Other Globals

(define hato-aliases #f)
(define hato-virtual-aliases #f)
(define hato-filter-cache #f)
(define hato-system-filter #f)
(define hato-rbl "sbl-xbl.spamhaus.org")
(define hato-rbl-cache (make-lru-cache equal: string=?))
(define hato-dkey-cache (make-lru-cache equal: string=?))
(define hato-dkey-policy-cache (make-lru-cache equal: string=?))
(define hato-dns-txt-cache (make-lru-cache equal: string=?))
(define hato-data-response (list 'response))

(define (hato-dir f)
  (string-append hato-base-directory f))

(define (hato-default-user-folder user)
  (string-append hato-spool-directory
                 (if (string? user) user (car (user-information user)))))

(define (hato-retry-time attempts)
  (let lp ((ls hato-retry-time-list) (i 0))
    (cond
      ((null? ls) #f)
      ((= i attempts) (car ls))
      (else (lp (cdr ls) (+ i 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates

(define (dns-blacklist? ip domain cache)
  (lru-ref! cache ip (lambda (ip)
                       (dns-lookup
                        (string-append
                         (string-intersperse
                          (reverse (string-split ip ".")) ".")
                         "."
                         domain)))))

(define valid-email-address?
  (let ((rx (regexp "^[-+_.A-Za-z0-9]+@[-+_.A-Za-z0-9]+$")))
    (lambda (addr) (and (string-match-positions rx addr) #t))))

(define (hato-local-domain? domain)
  (log-debug "hato-local-domain?: searching ~S in ~S" domain hato-local-domains)
  (member (string-downcase domain) hato-local-domains))

(define (hato-invalid-domain? domain . o)
  (letrec ((addr #f)
           (get-addr
            (lambda ()
              (or addr (set! addr (dns-lookup domain)) addr))))
    (not
     (and (or (not hato-verify-from-domain-existence?)
              (and (string? (get-addr)) (not (equal? "") (get-addr))))
          (or (not hato-verify-from-domain-matches-address?)
              (receive (local remote)
                  (tcp-addresses (if (pair? o) (car o) (current-input-port)))
                (equal? (get-addr) remote)))))))

(define (hato-valid-user? local domain)
  (if hato-virtual-users?
      (cond
       ((pair? hato-virtual-users) (member local hato-virtual-users))
       ((null? hato-virtual-users) #f)
       ((hash-table? hato-virtual-users)
        (hash-table-ref/default hato-virtual-users local #f))
       ((db? hato-virtual-users)
        (db-ref hato-virtual-users local))
       (else #t))
      (user-information local)))

(define (hato-local-address? addr)
  (and-let* ((domain (domain-part addr)))
    (hato-local-domain? domain)))

(define (hato-relay-allowed? domain port)
  (or hato-test-mode?
      (receive (local remote) (tcp-addresses port)
        (or (string=? local remote)
            (member remote hato-relay-domains)))))

(define (hato-relay-safe? msg-str)
  (let lp ((ls (with-input-from-string msg-str mime-headers->list))
           (sum 0))
    (if (null? ls)
        (< sum hato-max-received-count)
        (lp (cdr ls) (if (string-ci=? "Received" (caar ls)) (+ sum 1) sum)))))

;; return 'all (o=-) , 'some (o=~) or #f
;;   _domainkey   IN TXT "t=y; o=-; n=notes; r=emailAddress"
;;   "t=y; o=~; n=http://antispam.yahoo.com/domainkeys"
(define (hato-domain-keys-policy domain)
  (cond
   ((assoc domain hato-domain-keys-policy-alist)
    cdr)
   (else
    (lru-ref! hato-dkey-policy-cache domain
              (lambda (domain)
                (let* ((dkey-domain (string-append "_domainkey." domain))
                       (txt (dns-text dkey-domain))
                       (ls (map (cut string-split <> "=")
                                (map string-trim-both
                                     (string-split (or txt "") ";")))))
                  (cons (and-let* ((x (assoc "t" ls)))
                          (equal? (cadr x) "y"))
                        (and-let* ((x (assoc "o" ls)))
                          (case (string->symbol (cadr x))
                            ((-) 'all) ((~) 'some) (else #f))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Filters - suport ~/.hato/filter and .forward w/ rfc3028 support

(define (file->sexp-list file)
  (call-with-input-file file
    (lambda (p)
      (let lp ((ls '()))
        (let ((o (read p)))
          (if (eof-object? o)
              (reverse ls)
              (lp (cons o ls))))))))

(define (hato-run-filter log-file thunk)
  (with-output-to-file log-file
    (lambda ()
      (with-error-output-to-port (current-output-port)
        (lambda ()
          (with-timeout hato-run-filter-timeout thunk))))
    append:))

(define (hato-eval-user-filter user expr)
  (let ((env (hato-user-env user expand-user-dir)))
    (safe-eval
     `(lambda (addr mail)
        (current-address addr)
        (current-mail mail)
        (call-with-current-continuation
          (lambda (return)
            (letrec
                ((reject
                  (lambda o
                    (return (cons ',hato-data-response (cons 'reject o)))))
                 (discard
                  (lambda o
                    (return (cons ',hato-data-response (cons 'discard o)))))
                 (refuse
                  (lambda o
                    (return (cons ',hato-data-response (cons 'refuse o))))))
              ,expr))))
     environment: env)))

(define (hato-load-user-filter user file)
  (log-debug "hato-load-user-filter ~S ~S" user file)
  (let* ((expr (parameterize ((case-sensitive #f))
                 (file->sexp-list file)))
         (load-time (current-seconds)))
    ;;(log-debug "expr: ~S" expr)
    (cons load-time (hato-eval-user-filter user (cons 'begin expr)))))

(define (hato-user-filter-untimed user)
  (condition-case*
   (let* ((file (expand-user-dir user "~/.hato/filter"))
          (mtime (file-modification-time file))
          (cell
           (lru-ref! hato-filter-cache user
                     (lambda (user) (hato-load-user-filter user file)))))
     (if (and cell (> (car cell) mtime))
         (cdr cell)
         (let ((filt (hato-load-user-filter user file)))
           (lru-set! hato-filter-cache user filt)
           (cdr filt))))
   ;; shortcut, instead of testing for file existence and readability,
   ;; we just go ahead and try to load it, and catch file errors to
   ;; reduce the number of system calls
   ((exn file) #f)
   ((exn i/o) #f)
   (exn ()
        (log-error "error loading user filter ~S: ~A\n" user
                   (exception-message exn))
        #f)))

(define (hato-load-dot-forward user file)
  (call-with-input-file file
    (lambda (in)
      (let ((line (read-line in)))
        (cons
         (current-seconds)
         (case (string-ref line 0)
           ;;((#\|) (lambda (mail) line))
           ((#\#)
            (hato-eval-user-filter
             user
             (rfc3028-sieve->scheme (rfc3028-sieve-read in))))
           (else
            (let* ((addrs (map (lambda (x)
                                 (cond
                                  ((string-index x #\@)
                                   x)
                                  (else
                                   (string-append x "@" hato-host-name))))
                               (string-split line " \\t,")))
                   (res (if (and (pair? addrs) (null? (cdr addrs)))
                            (car addrs)
                            addrs)))
              (lambda (addr mail) res)))))))))

(define (hato-dot-forward-filter user)
  (condition-case*
   (let* ((file (expand-user-dir user "~/.forward"))
          (mtime (file-modification-time file))
          (cell
           (lru-ref! hato-filter-cache user
                     (lambda (user) (hato-load-dot-forward user file)))))
     (if (and cell (> (car cell) mtime))
         (cdr cell)
         (let ((filt (hato-load-dot-forward user file)))
           (lru-set! hato-filter-cache user filt)
           (cdr filt))))
   ((exn file) #f)
   ((exn i/o) #f)
   (exn ()
        (log-error "error loading user .forward ~S: ~A\n" user
                   (exception-message exn))
        #f)))

(define (hato-user-filter user)
  (with-timeout hato-load-filter-timeout
    (lambda ()
      (or (hato-user-filter-untimed user)
          (hato-dot-forward-filter user)))))

(define (hato-load-system-filter)
  (log-notice "loading system filter from ~A" hato-system-filter-file)
  (condition-case*
   (let ((sexp-ls (parameterize ((case-sensitive #f))
                    (file->sexp-list hato-system-filter-file))))
     (eval `(lambda (addr mail)
              (define current-address (make-parameter addr))
              (define current-mail (make-parameter mail))
              ,hato-sys-env-definitions
              (call-with-current-continuation
                (lambda (return)
                  (letrec
                      ((reject
                        (lambda o
                          (return (cons ',hato-data-response
                                        (cons 'reject o)))))
                       (discard
                        (lambda o
                          (return (cons ',hato-data-response
                                        (cons 'discard o)))))
                       (refuse
                        (lambda o
                          (return (cons ',hato-data-response
                                        (cons 'refuse o))))))
                    ,@sexp-ls))))))
   ((exn file) identity)
   ((exn i/o) identity)
   (exn ()
        (log-error "error loading system filter: ~A\n" (exception-message exn))
        identity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Queue handling

(define (enqueue-message str recipients . o)
  (define queue-maildir-error-limit 10)
  (let lp ((err 0))
    (condition-case*
     (let ((file (string-append hato-queue-directory
                                (generate-maildir-name))))
       (let* ((fd (file-open file (bitwise-ior open/rdwr open/creat open/excl)))
              (out (open-output-file* fd)))
         (display str out)
         (close-output-port out))
       (with-output-to-file (string-append file ".envelope")
         (lambda ()
           (write (list (cons 'time (current-seconds))
                        (cons 'recipients recipients)
                        (cons 'attempts (if (pair? o) (car o) 1)))))))
     ((exn i/o)
       (if (> err queue-maildir-error-limit)
         (log-critical "couldn't save mail to queue for ~S" recipients)
         (lp (+ err 1))))
     (exn ()
       (log-critical "couldn't save mail to queue for ~S: ~A" recipients
                     (exception-message exn))))))

(define (hato-run-queue)
  (let ((queue-files
         (condition-case
          (cddr (directory hato-queue-directory))
          (exn ()
           (log-critical "couldn't load queue files: ~A"
                         (exception-message exn))
           '()))))
    (let lp ((ls queue-files))
      (unless (null? ls)
        (condition-case*
         (if (string-suffix? ".envelope" (car ls))
           (and-let* ((envelope (call-with-input-file (car ls) read))
                      (last-sent (assq-ref envelope 'time))
                      (recipients (assq-ref envelope 'recipients))
                      (attempts (assq-ref envelope 'attempts 1))
                      (offset (hato-retry-time attempts))
                      (resend-time (+ last-sent offset))
                      (msg-file
                       (substring (car ls) 0 (- (string-length (car ls)) 9))))
             (if (and (< attempts hato-max-resend-attempts)
                      (>= (current-seconds) resend-time))
               (let ((msg (with-input-from-file msg-file read-string)))
                 (delete-file (car ls))
                 (delete-file msg-file)
                 (hato-relay msg recipients (+ 1 attempts))))))
         (exn ()
          (log-critical "error in run-queue: ~A" (exception-message exn))))
        (lp (cdr ls))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA Handling and delivery

;; A single address resolves into a (possibly empty) list of unique
;; destinations which are pairs of the form (rule . value).  Valid rules
;; are:
;;
;;   (file "/path/to/file" ["local-user-name"])
;;   (pipe "command line" [success-address [failure-address]])
;;   (relay "address" [prefix-headers])
;;   (reject|discard|refuse ["reason"])
;;
;; In the event of a reject, discard or refuse, processing will stop
;; immediately and that result will be returned as the sole list
;; element.
;;
;; Aliases and filters may result in internal loops, in which case the
;; loop will be terminated and all unique deliverable addresses that had
;; been expanded in the loop will be used.
;;
;; Processing follows the following steps:
;;
;;   1) implicit system filter (if not disabled)
;;      a) local@domain aliases
;;      b) local aliases
;;      c) @domain aliases
;;   2) system filter (if specified)
;;   3) if local@domain maps to a valid local (or virtual) user:
;;      a) user filter
;;      b) user .forward
;;      c) user default mailbox
;;
;; If none of the above steps resolve and the local-part can be split
;; according to the current separator (default '+'), then the steps are
;; repeated on the address with the rightmost specifier removed
;; (e.g. from bob+friends@humor.com to bob@humor.com).

(define (slurp-data dot)
  (let ((nl (call-with-output-string newline)))
    (let lp ((res '()))
      (let ((line (read-line)))
        (if (eof-object? line)
          (string-intersperse (reverse res) nl)
          (let ((len (string-length line)))
            (if (and (>= len 1) (eqv? dot (string-ref line 0)))
              (if (= 1 len)
                (string-intersperse (reverse res) nl)
                (lp (cons (substring line 1) res)))
              (lp (cons line res)))))))))

(define (hato-make-response-mail x)
  (cond
    ((pair? x) x)
    ((not (string-null? x)) `(Subject: "MAILER-DAEMON: Rejected Mail" Body: ,x))
    (else #f)))

(define (hato-deliver-local user msg . o)
  (log-debug "hato-deliver-local ~S" user)
  (let ((file (string-append hato-spool-directory "/" user))
        (headers (and (pair? o) (car o))))
    (log-debug "file: ~S headers: ~S" file headers)
    (mail-archive-add file msg #f headers #o600 (and hato-is-root? user))))

(define (hato-relay msg recipients . o)
  (let ((msg (sprintf
              "Received: by ~A with SMTP id ~A\r\n\t for <~A>; ~A\r\n~A"
              hato-host-name (gensym) (car recipients)
              (current-mbox-date-string) msg)))
    (if (hato-relay-safe? msg)
      (begin
        (log-notice "relaying mail to ~S\n" recipients)
        (let ((res
               (send-mail Host: #f Localhost: hato-host-name
                          Recipients: recipients Source: msg)))
          (when (pair? res)
            (log-warn "relay to ~S failed\n" res)
            (enqueue-message msg res (if (pair? o) (car o) 1)))))
      (log-warn "possible loop relaying to ~S\n" recipients))))

(define (hato-handle-data msg domain from to relay)
  (log-debug "hato-handle-data: ~S" domain)
  (let* ((headers (with-input-from-string msg mime-headers->list))
         (mail (make-mail msg headers)))
    (let lp ((ls to) (local '()) (relay relay) (files '()) (seen '()))
      (cond
       ((null? ls)
        (log-debug "deliver files: ~S, users: ~S relay: ~S" files local relay)
        (background
         (lambda () ; save to files, deliver to local users, relay outside
           (for-each (cut mail-archive-add <> msg #f headers) files)
           (for-each (cut hato-deliver-local <> msg) local)
           (if (pair? relay) (hato-relay msg relay))))
        "250 OK")
       ((member (car ls) seen) ; avoid loops
        (lp (cdr ls) local relay files seen))
       ((let ((to2 (and hato-system-filter
                        (hato-run-filter    ; 1st check system filter
                         (string-append hato-log-directory "/sys-filter.log")
                         (lambda () (hato-system-filter (car ls) mail))))))
          (and (or (string? to2) (pair? to2) (null? to2))
               (not (equal? to2 (car ls)))
               (not (and (pair? to2) (null? (cdr to2))
                         (equal? (car to2) (car ls))))
               to2))
        => (lambda (to2)
             (log-debug "system-filter: ~S => ~S" (car ls) to2)
             (cond
              ((and (pair? to2) (equal? (car to2) hato-data-response))
               (cond ;; XXXX duplicate code from user filter below
                ((eq? 'refuse (cadr to2))
                 (string-append
                  "550 "
                  (if (pair? (cddr to2)) (caddr to2) "Sorry")))
                (else ;; reject or discard
                 (log-info "reject: ~A (~A) => ~A"
                           from domain to)
                 ;; ... optionally sending a rejection notice
                 (if (pair? (cddr to2))
                     (let ((keywords
                            (hato-make-response-mail
                             (caddr to2))))
                       (if (pair? keywords)
                           (apply send-mail
                                  From: (car ls)
                                  To: from
                                  Host: #f
                                  keywords))))
                 "250 OK")))
              (else
               (lp (if (string? to2) (cons to2 (cdr to)) (append to2 (cdr to)))
                   local relay files (cons (car ls) seen))))))
       (else
        (let* ((ls2 (string-split (car ls) "@"))
               (user (car ls2))
               (domain (if (pair? (cdr ls2)) (cadr ls2) "localhost"))
               (seen (cons (car ls) seen)))
          (log-debug "user: ~A domain: ~A seen: ~S" user domain seen)
          (if (hato-local-domain? domain)
              (cond
               ((or (lookup-virtual-alias hato-virtual-aliases user domain)
                    (lookup-alias hato-aliases user domain))
                => (lambda (alias)
                     (lp (if (pair? alias)
                             (append alias (cdr ls))
                             (cons alias (cdr ls)))
                         local relay files seen)))
               ((hato-user-filter user)
                => (lambda (filt)
                     (log-debug "found user filter")
                     (let ((res (hato-run-filter
                                 (expand-user-dir user "~/.hato/filter.log")
                                 (lambda () (filt (car ls) mail)))))
                       (log-debug "filter result: ~S" res)
                       (cond
                        ((pair? res)
                         (if (equal? (car res) hato-data-response)
                             (case (cadr res)
                               ((reject discard) ; terminate immediately
                                (log-info "reject: ~A (~A) => ~A"
                                          from domain to)
                                ;; ... optionally sending a rejection notice
                                (if (pair? (cddr res))
                                    (let ((keywords
                                           (hato-make-response-mail
                                            (caddr res))))
                                      (if (pair? keywords)
                                          (apply send-mail
                                                 From: (car ls)
                                                 To: from
                                                 Host: #f
                                                 keywords))))
                                "250 OK")
                               ((refuse) ; terminate immediately with notice
                                (string-append
                                 "550 "
                                 (if (pair? (cddr res)) (caddr res) "Sorry")))
                               (else ; this shouldn't happen
                                (log-warn "unknown response: ~S\n" (cdr res))
                                (lp (cdr ls) local relay files seen)))
                             (lp (cdr ls) local relay
                                 (append
                                  (map (cut expand-user-dir user <>) res)
                                  files)
                                 seen)))
                        ((string? res) ; XXXX maybe handle "pipe:..." etc.
                         (if (string-index res #\@)
                             (lp (cons res (cdr ls)) local relay files seen)
                             (lp (cdr ls) local relay
                                 (cons (expand-user-dir user res) files)
                                 seen)))
                        (else
                         (if (and res (defined? res))
                             (log-warn "unknown filter result: ~S\n" res))
                         (lp (cdr ls) (cons user local) relay files seen))))))
               (else
                (lp (cdr ls) (cons user local) relay files seen)))
              (lp (cdr ls) local (cons (car ls) relay) files seen))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main SMTP command loop

(define (hato-repl domain from to relay)
  (flush-output)
  (define (get-command str)
    (let ((i (string-index str #\space)))
      (if i
        (substring str 0 i)
        str)))
  (define (get-colon-arg str)
    (and-let* ((start (string-index str #\:)))
      (let ((end (string-index str #\: (+ start 1))))
        (if end
          (substring str (+ start 1) end)
          (substring str (+ start 1))))))
  (define (get-email str)
    (and-let* ((addr (get-colon-arg str)))
      (string-trim-right (string-trim addr #\<) #\>)))
  (define (run-eval str)
    (let ((res (eval (call-with-input-string str read))))
      (display "250 ") (write res) (newline)
      (hato-repl domain from to relay)))
  (let ((line (read-line)))
    (cond
      ((eof-object? line)
       #f)
      ((and hato-test-mode? (not (equal? line ""))
            (eqv? #\( (string-ref line 0)))
       (run-eval line))
      (else
       (let* ((command (get-command line))
              (command-sym (string->symbol (string-upcase command))))
         (case command-sym
           ((EHLO HELO)
            (let ((len (string-length line)))
              (cond
                ((> len 5)
                 (let ((domain (string-trim-both (substring line 5))))
                   (cond
                     ((hato-invalid-domain? domain)
                      (log-warn "invalid domain: ~A" domain)
                      (pr "550 Sorry")
                      (unless hato-disconnect-on-error?
                        (hato-repl domain from to relay)))
                     ((and (string-ci=? domain "localhost")
                           (not hato-test-mode?)
                           (receive (local remote)
                               (tcp-addresses (current-input-port))
                             (not (or (equal? local remote)
                                      (equal? local "127.0.0.1")))))
                      (log-warn "pretending to be localhost from: ~A" from)
                      (pr "550 Sorry")
                      (unless hato-disconnect-on-error?
                        (hato-repl domain from to relay)))
                     (else
                      (display "250 ") (display hato-host-name) (pr " hi")
                      (hato-repl domain #f '() relay)))))
                (else
                 (pr "500 Must specify a domain")
                 (unless hato-disconnect-on-error?
                   (hato-repl domain from to relay))))))
           ((MAIL)
            (cond
              (from
               (pr "500 Already specified sender")
               (unless hato-disconnect-on-error?
                 (hato-repl domain from to relay)))
              (else
               (let ((from (get-email line)))
                 ;; XXXX verify from + domain (black/white lists and SPF)
                 (pr "250 OK")
                 (hato-repl domain from to relay)))))
           ((RCPT)
            (cond
              ((not from)
               (pr "500 Must specify sender first")
               (unless hato-disconnect-on-error?
                 (hato-repl domain from to relay)))
              (else
               (let ((addr (get-email line)))
                 (cond
                   ((hato-local-address? addr)
                    (pr "250 OK")
                    (hato-repl domain from (cons addr to) relay))
                   ((and (valid-email-address? addr)
                         (not (hato-local-domain? (domain-part addr)))
                         (hato-relay-allowed? domain (current-input-port)))
                    (log-debug "relaying ~S" addr)
                    (pr "250 OK")
                    (hato-repl domain from to (cons addr relay)))
                   (else
                    (pr "550 Sorry")
                    (unless hato-disconnect-on-error?
                      (hato-repl domain from to relay))))))))
           ((DATA)
            (cond
              ((not to)
               (pr "500 Must specify recipient first")
               (unless hato-disconnect-on-error?
                 (hato-repl domain from to relay)))
              (else
               (pr "354 Start data input; end with CRLF.CRLF")
               (let* ((res (hato-handle-data
                            (slurp-data #\.) domain from to relay))
                      (code (cond
                             ((number? res) res)
                             ((string? res)
                              (or (string->number (car (string-split res)))
                                  550))
                             (else 550))))
                 (log-debug "DATA result: ~S" res)
                 (pr res)
                 (if (or (and (number? code) (< code 400))
                         (not hato-disconnect-on-error?))
                   (hato-repl domain from to relay))))))
           ((RSET)
            (pr "250 OK")
            (hato-repl #f #f '() '()))
           ((NOOP)
            (pr "250 OK")
            (hato-repl domain from to relay))
           ((QUIT)
            (pr "250 OK"))
           ((VRFY EXPN)
            (pr "502 Not Implemented")
            (hato-repl domain from to relay))
           ((EVAL)
            (cond
              (hato-test-mode?
               (run-eval (substring line 4)))
              (else
               (pr "500 Unknown Command")
               (unless hato-disconnect-on-error?
                 (hato-repl domain from to relay)))))
           (else
            (pr "500 Unknown Command")
            (unless hato-disconnect-on-error?
              (hato-repl domain from to relay)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial SMTP connection handler

(define (hato-banner)
  (display "220 ") (display hato-host-name) (pr " SMTP Ready"))

(define (hato-handle)
  (condition-case*
   ((lambda (response) (if (string? response) (pr response)))
    (receive (local remote) (tcp-addresses (current-input-port))
      (log-info "connection from ~A on ~A" remote local)
      (let ((local? (or (string=? local remote)
                        (member remote hato-local-ip-addresses))))
        ;;(log-debug "local? ~S" local?)
        ;; some quick rejection tests before starting a full session
        (cond
         ((and (not local?)
               hato-rbl
               (dns-blacklist? remote hato-rbl hato-rbl-cache))
          (log-warn "disconnect: black-list: ~A" remote)
          "550 sorry")
         (else
          (let ((wait (if local? 0 (random hato-jump-the-gun-limit))))
            (if (positive? wait)
                (begin
                  (log-debug "jump-the-gun: wait ~S" wait)
                  (thread-sleep! wait)))
            (cond
             ((char-ready?)             ; jump-the-gun test
              (log-warn "disconnect: jumped-the-gun in ~A seconds" wait)
              "550 sorry")
             (else
              (hato-banner)
              (with-timeout
                  hato-connection-timeout
                (lambda () (hato-repl #f #f '() '())))
              "221 bye"))))))))
   (exn ()
        (if hato-debug?
            (begin
              (print-call-chain (current-error-port))
              (print-error-message exn (current-error-port))
              (newline (current-error-port))))
        (log-error "hato-handle: ~A" (exception-message exn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hato-kill-running)
  (let ((pid (read-from-file hato-pid-file #f)))
    (cond
     ((and (number? pid) (running-process-id? pid "hato"))
      (log-notice "shutting down server with process ID: ~A" pid)
      (condition-case (process-signal pid)
        (exn ()
             (log-error "couldn't SIGTERM process ~A, trying SIGKILL" pid)
             (condition-case (process-signal pid signal/kill)
               (exn () (log-critical "couldn't SIGKILL process"))))))
     ((number? pid)
      (log-notice "removing stale lock file for pid ~A" pid)
      (condition-case (delete-file hato-pid-file)
        (exn ()
             (log-warn "couldn't remove stale lock file: ~A"
                       (exception-message exn)))))
     (else
      (log-notice "no process to kill")))))

(define (hato-quit signal)
  (log-notice "shutting down: ~S" (current-process-id))
  (condition-case (if (file-exists? hato-pid-file)
                      (delete-file hato-pid-file))
    (exn ()
         (log-notice "couldn't delete pid file: ~A" (exception-message exn))))
  (log-close)
  (exit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handle options and config

(define-macro (default . o) ; like or but checks for first defined value
  (cond
    ((null? o) #f)
    ((null? (cdr o)) (car o))
    (else
     (let ((tmp (gensym)))
       `(let ((,tmp ,(car o)))
          (if (eq? ,tmp undef) (default ,@(cdr o)) ,tmp))))))

(define (setup-file-system)
  (create-directory* hato-spool-directory)
  (create-directory* hato-log-directory)
  (create-directory* hato-run-directory)
  (create-directory* hato-queue-directory)
  (log-close)
  (log-open)
  (if (output-port? current-log-port)
    (current-error-port current-log-port)))

(define (hato-set-signal-handlers! args)
  (set-signal-handler! signal/pipe #f)
  (set-signal-handler! signal/kill hato-quit)
  (set-signal-handler! signal/term hato-quit)
  (set-signal-handler! signal/int hato-quit)
  (set-signal-handler! signal/hup (lambda (i) (reset-config args))))

(define (user-id->number id)
  (if (not (string? id))
      id
      (cond
       ((string->number id))
       ((user-information id) => caddr)
       (else #f))))

(define (group-id->number id)
  (if (not (string? id))
      id
      (cond
       ((string->number id))
       ((group-information id) => caddr)
       (else #f))))

(define (reset-config args)
  (let-args args
      ((debug? "d|debug" undef)
       (no-fork? "no-fork" undef)
       (log-level "log-level=i" undef)
       (port "p|port=i" undef)
       (user-id "user-id=s" undef)
       (group-id "group-id=s" undef)
       (disconnect-on-error? "disconnect" undef)
       (max-received-count "max-received=i" undef)
       (max-requests "max-requests=i" undef)
       (filter-cache-limit "filter-cache-limit=i" undef)
       (host-name "h|host=s" undef)
       (relay-domains "r|relay=s" undef)
       (local-ip-addresses "local-ips|local-ip-addresses=s" undef)
       (local-domains "local-domains=s" undef)
       (log-file "log|log-file=s" undef)
       (pid-file "pid|pid-file=s" undef)
       (system-filter-file "filter|filter-file=s" undef)
       (queue-delay "queue-delay=i" undef)
       (max-resend-attempts "max-resend-attempts=i" undef)
       (retry-time-list "retry-time-list=s" undef)
       (time-zone "time-zone=s" undef)
       (virtual-users? "virtual|virtual-users" undef)
       ;; non-config-file options
       (test-mode? "t|test" #f)
       (run-queue? "runq|run-queue" #f)
       (sendmail? "sendmail" #f)
       (kill? "k|kill" #f)
       (base-directory
        "base|base-directory=s"
        (if hato-is-root? "/usr/local/hato/"
            (string-append (or (getenv "HOME") ".") "/hato/")))
       (config-path
        "c|config-path=s"
        (if hato-is-root? "/etc/hato/" (make-pathname base-directory "etc/")))
       ;; optional session files for test-mode
       . arguments)

    (letrec ((cfg (conf-load-cascaded
                   (string-split config-path ":")
                   "config.scm"))
             (get (lambda (v k d) (default v (conf-get cfg k undef) d)))
             (get-list (lambda (v k d) (default v (conf-get-list cfg k undef) d))))
      ;; precedence: command-line, env, config, default
      ;; XXXX add cleaner integration of command-line opts and config

      (set! hato-debug? (get debug? 'debug? #f))
      (set! hato-no-fork? (get no-fork? 'no-fork? #f))
      (set! current-log-level (get log-level 'log-level (if hato-debug? 7 6)))
      (set! hato-user-id (user-id->number (get user-id 'user-id #f)))
      (set! hato-group-id (group-id->number (get group-id 'group-id #f)))
      (set! hato-base-directory (path-as-directory base-directory))
      (set! hato-spool-directory
            (path-as-directory
             (or (getenv "HATO_SPOOL_DIRECTORY")
                 (conf-get cfg 'spool-directory)
                 (if hato-is-root?
                     "/var/mail/"
                     (hato-dir "var/mail/")))))
      (set! hato-queue-directory
            (path-as-directory
             (or (getenv "HATO_QUEUE_DIRECTORY")
                 (conf-get cfg 'queue-directory)
                 (if hato-is-root?
                     "/var/hato/queue/"
                     (hato-dir "var/queue/")))))
      (set! hato-log-directory
            (path-as-directory
             (or (getenv "HATO_LOG_DIRECTORY")
                 (conf-get cfg 'log-directory)
                 (if hato-is-root?
                     "/var/log/hato/"
                     (hato-dir "var/log/")))))
      (set! hato-run-directory
            (path-as-directory
             (or (getenv "HATO_RUN_DIRECTORY")
                 (conf-get cfg 'run-directory)
                 (if hato-is-root? "/var/run/hato/" (hato-dir "var/run/")))))
      (set! hato-virtual-home-directory
            (path-as-directory
             (or (getenv "HATO_VIRTUAL_HOME_DIRECTORY")
                 (conf-get cfg 'virtual-home-directory)
                 (hato-dir "home/"))))
      (set! current-log-file
            (and (not test-mode?)
                 (get log-file 'log-file
                      (string-append hato-log-directory "hato.log"))))
      (set! hato-pid-file
            (get pid-file 'pid-file
                 (string-append hato-run-directory "hato.pid")))
      (setup-file-system)

      (set! hato-queue-delay (get queue-delay 'queue-delay 600))
      (set! hato-port (get port 'port (if hato-is-root? 25 5025)))
      (set! hato-time-zone (get time-zone 'time-zone (system-line "date +%z")))
      (set! hato-virtual-users? (get virtual-users? 'virtual-users? #f))
      (set! hato-virtual-users (conf-load-table (conf-get cfg 'virtual-users)))
      (set! hato-disconnect-on-error?
            (get disconnect-on-error? 'disconnect-on-error?
                 (not test-mode?)))
      (set! hato-max-received-count
            (get max-received-count 'max-received-count 20))
      (set! hato-max-requests (get max-requests 'max-requests 100))
      (set! hato-max-resend-attempts
            (get max-resend-attempts 'max-resend-attempts 4))
      (set! hato-retry-time-list
            (if (defined? retry-time-list)
              (map string->number (string-split retry-time-list ","))
              (or (conf-get cfg 'retry-time-list)
                  ;; 1 hour, 4 hours, 1 day, 4 days
                  '(3600 14400 86400 345600))))
      (set! hato-jump-the-gun-limit (conf-get cfg 'jump-the-gun-limit 0))
      (set! hato-filter-cache-limit
            (get filter-cache-limit 'filter-cache-limit 4194304))

      (set! hato-host-name
            (get host-name 'host-name
                 (or (getenv "HATO_HOST_NAME")
                     (cond
                      ((get-list local-domains 'local-domains #f)
                       => (lambda (ls) (and (pair? ls) (car ls))))
                      (else #f))
                     (string-trim-both
                      (system-line "hostname")))))
      (set! hato-relay-domains
            (if (defined? relay-domains)
              (string-split relay-domains ":")
              (conf-get cfg 'relay-domains '())))
      (set! hato-local-ip-addresses
            (if (defined? local-ip-addresses)
              (string-split local-ip-addresses ":")
              (conf-get cfg 'local-ip-addresses '("127.0.0.1"))))
      (set! hato-local-domains
            (map string-downcase
                 (or (get-list local-domains 'local-domains #f)
                     (sub-domains hato-host-name))))
      (if (string? hato-local-domains)
        (set! hato-local-domains (string-split hato-local-domains ",")))
      (set! hato-local-domains (cons "localhost" hato-local-domains))

      ;; We limit the filter cache on the actual memory footprint of
      ;; each user's compiled filter (default 4MB limit).
      (set! hato-filter-cache
            (make-lru-cache equal: string=?
                            compute-size: (lambda (u f) (object-size f))
                            limit: hato-filter-cache-limit))
      (set! hato-aliases
            (and-let* ((file (find-file-in-paths
                              "aliases"
                              (list (hato-dir "etc") "/etc" "/etc/mail"))))
              (log-debug "loading alias file: ~A" file)
              (load-alias-file file)))
      (set! hato-virtual-aliases
            (and-let* ((file (find-file-in-paths
                              "virtual"
                              (list (hato-dir "etc") "/etc" "/etc/mail"))))
              (log-debug "loading virtual alias file: ~A" file)
              (load-alias-file file)))
      (set! hato-system-filter-file
            (get system-filter-file 'system-filter-file
                 (find-file-in-paths "filter" (string-split config-path ":"))))
      (set! hato-system-filter (and hato-system-filter-file
                                    (hato-load-system-filter)))
      (set! hato-arguments arguments)

      (cond ; return run-mode
        (test-mode? 'test-mode)
        (run-queue? 'run-queue)
        (sendmail?  'sendmail)
        (kill?      'kill)
        (else ; default from name of executable
         (let ((name (pathname-strip-directory (car (argv)))))
           (case (string->symbol name)
             ((runq) 'run-queue)
             ((rmail sendmail) 'sendmail)
             (else 'daemon))))))))

(define (main args)
  ;; parse command-line options and config file
  (let ((run-mode (reset-config args)))
    (log-debug "starting hato in run-mode: ~A, chicken-version: ~A"
               run-mode (chicken-version))
    (case run-mode
      ((test-mode)
       (cond
         ((and (file-suid? (car (argv)))
               (not (= (current-user-id) (file-owner (car argv)))))
          (print "Sorry, you must own an suid file to run it in test mode.")
          (exit 1))
         (else
          (set! hato-test-mode? #t)
          (hato-banner)
          (if (pair? hato-arguments)
            (with-input-from-file (car hato-arguments)
              (lambda () (hato-repl #f #f '() '())))
            (hato-repl #f #f '() '()))
          (exit 0))))
      ((run-queue)
       (hato-run-queue))
      ((sendmail)
       (hato-handle-data (slurp-data #\.)
                         "localhost"
                         (or (getenv "EMAIL")
                             (string-append (get-user-name)"@"hato-host-name))
                         hato-arguments
                         '()))
      ((kill)
       (hato-kill-running))
      (else

       ;; run as a standard daemon
       (daemonize
        name: "hato"
        pid-file: hato-pid-file
        user-id: hato-user-id
        group-id: hato-group-id
        tcp-port: hato-port
        tcp-handler: hato-handle
        tcp-max-requests: hato-max-requests
        tcp-debug?: hato-debug?
        init-thunk: (lambda ()
                      (hato-set-signal-handlers! args)
                      (background-periodic hato-queue-delay hato-run-queue)) 
        )

       ))))

(main (command-line-arguments))

