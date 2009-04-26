#!/usr/local/bin/csi -script
;;;; hato-fetch.scm -- fetchmail replacement
;;
;; Copyright (c) 2005-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-library hato-filter-env)

(module hato-fetch (main)

(import (except scheme + - * / = > < >= <= number->string string->number
                eqv? equal? exp log sin cos tan atan acos asin expt sqrt
                quotient modulo remainder numerator denominator abs min max
                gcd lcm positive? negative? odd? even? zero? exact? inexact?
                floor ceiling truncate round inexact->exact exact->inexact
                number? complex? real? rational? integer? real-part imag-part
                magnitude))
(import (except chicken add1 sub1 signum bitwise-and bitwise-ior bitwise-xor
                bitwise-not arithmetic-shift))
(import (except extras random randomize))
(require-extension
 utils regex data-structures ports posix numbers
 srfi-1 srfi-13 srfi-69
 tokyocabinet let-args domain-keys safe-eval
 hato-archive hato-uri hato-pop hato-imap hato-smtp hato-mime
 hato-utils hato-config hato-date hato-prob hato-daemon hato-db
 hato-log)
(import (only hato-filter-env make-mail))

(define-logger (current-log-level set-current-log-level! info)
  (log-emergency ; the server is on fire!!!           
   log-alert     ; couldn't write to user mailbox     
   log-critical  ; couldn't run 'dig' executable      
   log-error     ; error loading user filter          
   log-warn      ; invalid smtp command; relay failed 
   log-notice    ; saved to file/relayed to address   
   log-info      ; loaded alias file                  
   log-debug))   ; spam-probability: 0.5

(define *program-name* "hato-fetch")
(define-syntax read-version
  (er-macro-transformer
   (lambda (e r c) (call-with-input-file "VERSION" read-line))))
(define *program-version* (read-version))

(define (expand-path path)
  (expand-user-path (current-user-id) path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list utilities

(define (unique/sorted ls . o)
  (if (null? ls)
      '()
      (let ((eq (if (pair? o) (car o) equal?)))
        (let lp ((ls (cdr ls)) (res (list (car ls))) (cur (car ls)))
          (cond
           ((null? ls)
            (reverse res))
           ((eq cur (car ls))
            (lp (cdr ls) res cur))
           (else
            (lp (cdr ls) (cons (car ls) res) (car ls))))))))

(define (expand-interval-steps ls . o)
  (let ((full-interval (if (pair? o) (car o) (apply lcm ls))))
    (define (multiples n)
      (let lp ((mult 0) (res '()))
        (let ((mult (+ mult n)))
          (if (>= mult full-interval)
              res
              (lp mult (cons mult res))))))
    (unique/sorted (sort (append-map multiples ls) <))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uri utilities

(define (source->canonical-uri source)
  (let ((proto (conf-get source 'protocol)))
    (case proto
      ((file)
       (string-append "file:" (conf-get source 'file)))
      ((imap imaps pop pop3 pop3s)
       (conc proto "://" (conf-get source 'username) "@"
             (conf-get source 'host)
             (cond ((conf-get source 'mailbox)
                    => (lambda (mbox) (conc "/" mbox)))
                   (else ""))))
      (else
       #f))))

(define (parse-input-uri src)
  (let ((uri (string->uri src)))
    (cond
     (uri
      (case (uri-scheme uri)
        ((alias) `((protocol alias) (alias ,(uri-path uri))))
        ((file maildir mh mbox)
         `((protocol file) (file ,(uri-decode (uri-path uri)))))
        ((pipe)  `((protocol pipe) (command ,(uri-decode (uri-path uri)))))
        ((port)  `((protocol port) (port ,(current-input-port))))
        ((test)  `((protocol test)
                   (subject ,(cond ((uri-path uri) => uri-decode)
                                   (else #f)))))
        ((null)  `((protocol null)))
        ((pop pop3 pop3s)
         `((protocol pop)
           (no-history? #t)
           (host ,(uri-host uri))
           ,@(cond ((uri-user uri) => (lambda (u) `((username ,u)))) (else '()))
           ))
        ((imap imaps)
         `((protocol ,(uri-scheme uri))
           (no-history? #t)
           (host ,(uri-host uri))
           ,@(cond ((uri-user uri) => (lambda (u) `((username ,u)))) (else '()))
           ,@(cond ((uri-port uri) => (lambda (p) `((port ,p)))) (else '()))
           ,@(cond ((uri-path uri)
                    => (lambda (m) `((mailboxes ,(uri-decode (substring m 1) #t)))))
                   (else '()))
           ,@(cond ((uri-fragment uri) => (lambda (n) `((messages ,n)))) (else '()))
           ,@(cond ((uri-query uri)
                    => (lambda (q)
                         `((search
                            ,(append-map (lambda (x) (list (car x) (cdr x)))
                                         (uri-query->alist q))))))
                   (else '()))
           ))
        (else (die "unknown mail source scheme: ~S" (uri-scheme uri)))))
     (else
      (cond
       ((equal? "-" src)
        `((protocol port) (port ,(current-input-port))))
       ((eqv? #\| (string-ref src 0))
        `((protocol pipe) (command ,(substring src 1))))
       ((eqv? #\: (string-ref src 0))
        `((protocol alias) (alias ,(substring src 1))))
       (else
        `((protocol file) (file ,src))))))))

(define (parse-output-uri dest)
  (let ((uri (string->uri dest)))
    (cond
     (uri
      (case (uri-scheme uri)
        ((alias)    `((protocol alias) (alias ,(uri-path dest))))
        ((file)     `((protocol file) (file ,(uri-decode (uri-path uri)))))
        ((maildir)  `((protocol maildir) (file ,(uri-decode (uri-path uri)))))
        ((mh)       `((protocol mh) (file ,(uri-decode (uri-path uri)))))
        ((mbox)     `((protocol mbox) (file ,(uri-decode (uri-path uri)))))
        ((port)     `((protocol port) (port ,(current-output-port))))
        ((pipe)     `((protocol pipe)
                      (command ,(cond ((uri-path uri) => uri-decode)
                                      (else "procmail")))))
        ((smtp)     `((protocol smtp) (address ,(uri-path uri))))
        ((hato)   `((protocol hato) (file ,(uri-path uri))))
        ((null)     `((protocol null)))
        ((imap imaps)
         `((protocol ,(uri-scheme uri))
           (no-history? #t)
           (host ,(uri-host uri))
           ,@(cond ((uri-port uri) => (lambda (p) `((port ,p)))) (else '()))
           ))
        (else
         (die "unknown destination scheme: ~S" (uri-scheme uri)))))
     (else
      (cond
       ((equal? "-" dest)
        `((protocol port) (port ,(current-output-port))))
       ((eqv? #\| (string-ref dest 0))
        `((protocol pipe) (command ,(substring dest 1))))
       ((eqv? #\: (string-ref dest 0))
        `((protocol alias) (alias ,(substring dest 1))))
       ((substring-index "@" dest)
        `((protocol smtp) (address ,dest)))
       (else
        `((protocol file) (file ,dest))))))))

(define (parse-filter str)
  (cond
   ((string-index str #\=)
    => (lambda (i)
         (list (string->symbol (string-downcase (substring str 0 i)))
               (substring str (+ i 1)))))
   (else
    (string->symbol str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input handlers

(define (wrap-no-run-input-handler src proc)
  (lambda (kons knil)
    (if (conf-get src 'no-run?)
        knil
        (proc kons knil))))

(define (wrap-filter filter kons text knil)
  (let ((headers (call-with-input-string text mime-headers->list)))
    (if (filter text headers)
        (kons text headers knil)
        knil)))

(define (mime-sent headers)
  (or (cond ((mime-ref headers "Sent-Date")
             => parse-date)
            (else #f))
      (cond ((mime-ref headers "Date") => parse-date)
            (else #f))))

(define make-test-message
  (let ((n 0))
    (lambda (config)
      (set! n (+ n 1))
      (call-with-output-string
        (lambda (out)
          (smtp-write-message
           out
           From: (or (conf-get config 'from)
                     (conc (current-user-name) "@" (get-host-name)))
           To: (or (conf-get config 'to)
                   (conc (current-user-name) "@" (get-host-name)))
           Subject: (or (conf-get config 'subject)
                        (string-append "test message " (number->string n)))
           Body: (or (conf-get config 'body) "testing")
           Newline: (call-with-output-string newline)))))))

(define (mbox-pattern->regexp str)
  (let ((len (string-length str)))
    (let lp ((i 0) (res '()))
      (if (>= i len)
          (list->string (reverse res))
          (let ((c (string-ref str i)))
            (case (string-ref str i)
              ((#\*) (lp (+ i 1) `(#\* #\. ,@res)))
              ((#\%) (lp (+ i 1) `(#\* #\] #\/ #\^ #\[ ,@res)))
              ((#\. #\? #\^ #\$ #\[ #\] #\( #\) #\{ #\} #\\ #\|)
               (lp (+ i 1) `(,c #\\ ,@res)))
              (else
               (lp (+ i 1) `(,c ,@res)))))))))

(define (wrap-mail-text text . o)
  (let ((headers (call-with-input-string text
                   mime-headers->list)))
    (if (pair? headers)
        text
        (let ((subject (if (pair? o) (car o) "enclosed message")))
          (make-test-message `(((subject ,subject) (body ,text))))))))

(define (port-input-handler src filter)
  (wrap-no-run-input-handler
   src
   (lambda (kons knil)
     (let ((text (wrap-mail-text (read-all (conf-get src 'port)))))
       (wrap-filter filter kons text knil)))))

(define (file-input-handler src filter)
  (let ((file (conf-get src 'file)))
    (if (not (mail-archive-format file))
        (die "not a valid mail archive: ~S" file))
    (wrap-no-run-input-handler
     src
     (lambda (kons knil)
       (let ((res (mail-archive-fold
                   file
                   (lambda (msgid text files+res)
                     (let ((headers (call-with-input-string text
                                      mime-headers->list)))
                       (if (filter text headers)
                           (cons (cons msgid (car files+res))
                                 (kons text headers (cdr files+res)))
                           files+res)))
                   (cons '() knil))))
         (if (conf-get src 'delete?)
             (mail-archive-remove file (car res)))
         (if (conf-get src 'delete-after)
             (log-notice "delete-after not supported for file: sources"))
         (cdr res))))))

(define (test-input-handler src filter)
  (wrap-no-run-input-handler
   src
   (lambda (kons knil)
     (wrap-filter filter kons (make-test-message src) knil))))

(define (null-input-handler src filter)
  (lambda (kons knil)
    knil))

(define (pipe-input-handler src filter)
  (wrap-no-run-input-handler
   src
   (lambda (kons knil)
     (let ((command (conf-get src 'command)))
       (receive (in out pid) (process command)
         (close-output-port out)
         (wrap-filter filter
                      kons
                      (wrap-mail-text (read-all in)
                                      (string-append "output of " command))
                      knil))))))

(define (pop3-input-handler src filter)
  (let* ((host (or (conf-get src 'host)
                   (conf-get src 'smtp-host "localhost")))
         (port (or (conf-get src 'port)
                   (if (eq? 'pop3s (conf-get src 'protocol))
                       443
                       110)))
         (debug? (conf-get src 'debug?))
         (delete? (conf-get src 'delete?))
         (delete-after (conf-get src 'delete-after))
         (user (or (conf-get src 'username) (current-user-name)))
         (pass
          (or (conf-get src 'password)
              (read-password
               (sprintf "Password (POP3) for ~A@~A: " user host)
               (lambda (str)
                 (let ((res (pop3-connect host user str debug? port)))
                   (cond
                    (res (pop3-disconnect res) #t)
                    (else #f))))))))
    (lambda (kons knil)
      (log-notice "polling pop3://~A@~A" user host)
      (if (conf-get src 'no-run?)
          knil
          (let* ((pop (pop3-connect host user pass debug? port))
                 (src (conf-restore src))
                 (limit (car (pop3-stat pop)))
                 (limit-uid (pop3-uidl pop limit))
                 (last-fetch (conf-get src 'last-fetch 0))
                 (last-id (conf-get src 'last-id))
                 (last-uid (conf-get src 'last-uid))
                 (last-msgid (conf-get src 'last-msgid))
                 (verify-all-times? (conf-get src 'verify-all-times?))
                 (start
                  (if (and last-id
                           last-uid
                           (equal? last-uid
                                   (if (eqv? last-id limit)
                                       limit-uid
                                       (pop3-uidl pop last-id))))
                      (+ last-id 1)
                      (or (and last-uid
                               (let ((uid
                                      (find (lambda (x)
                                              (equal? last-uid (cdr x)))
                                            (or (pop3-uidl pop) '()))))
                                 (and uid (car uid))))
                          1))))
            ;; try to pick up from where we left off
            (let lp ((i start)
                     (res knil)
                     (failures '())
                     (times '())
                     (msgid #f))
              (define (save n del-count)
                (conf-save src
                           `((version ,*program-version*)
                             (last-fetch ,(current-seconds))
                             (failures ,failures)
                             (deleted ,del-count)
                             (last-id ,n)
                             ,@(if (and (= n limit) limit-uid)
                                   `((last-uid ,limit-uid))
                                   '())
                             ,@(if msgid `((last-msgid ,msgid)) '()))))
              (cond
               ((> i limit)
                (let ((del-count
                       (condition-case
                           (cond
                            ((real? delete-after)
                             (let* ((offset (* delete-after 60 60 24))
                                    (dt (- (current-seconds) offset)))
                               ;; delete messages we've already fetched
                               ;; and know the sent time for...
                               (+ (fold
                                   (lambda (x acc)
                                     (if (and (cdr x)
                                              (< (if (vector? (cdr x))
                                                     (local-time->seconds
                                                      (cdr x))
                                                     (cdr x))
                                                 dt)
                                              (not (memv (car x) failures))
                                              (pop3-dele pop (car x)))
                                         (+ acc 1)
                                         acc)) 
                                   0
                                   times)
                                  ;; ... plus earlier messages that we
                                  ;; need to scan on (assume they're in
                                  ;; order)
                                  (call-with-current-continuation
                                    (lambda (return)
                                      (fold
                                       (lambda (x acc)
                                         (let* ((headers
                                                 (or (pop3-headers pop x) '()))
                                                (sent (mime-sent headers)))
                                           (if (and
                                                sent
                                                (< (if (vector? sent)
                                                       (local-time->seconds
                                                        sent)
                                                       sent)
                                                   dt)
                                                (pop3-dele pop x))
                                               (+ acc 1)
                                               (if verify-all-times?
                                                   acc
                                                   (return acc)))))
                                       0
                                       (iota (- start 1) 1)))))))
                            (delete?
                             (do ((i start (+ i 1))
                                  (count 0 (if (and (not (memv i failures))
                                                    (pop3-dele pop i))
                                               (+ count 1)
                                               count)))
                                 ((> i limit) count)))
                            (else 0))
                         (exn () #f))))
                  (condition-case (pop3-disconnect pop) (exn () #f))
                  (save (- limit del-count) del-count)
                  res))
               (else
                (let ((text
                       (condition-case (pop3-retr->string pop i)
                         (exn ()
                              (log-error "couldn't retrieve ~S" i)
                              #f))))
                  (if (not text)
                      ;; couldn't retrieve i, prematutely terminate
                      (begin
                        (save (- i 1) 0)
                        (pop3-disconnect pop))
                      ;; process this message
                      (let* ((headers (call-with-input-string text
                                        mime-headers->list))
                             (sent (mime-sent headers))
                             (msgid (cond ((mime-ref headers "Message-Id")
                                           => string-trim-both)
                                          (else #f)))
                             (times (if delete-after
                                        (cons (cons i sent) times)
                                        times))
                             (res2
                              (and text
                                   (condition-case (filter text headers)
                                     (exn () #t))
                                   (condition-case (kons text headers res)
                                     (exn () #f)))))
                        (if res2
                            (lp (+ i 1) res2 failures times msgid)
                            (lp (+ i 1) res (cons i failures) times msgid)))
                    ))))))))))

(define (imap-message-id->uid imap msgid)
  (let ((res (imap-search/uid imap `(HEADER "Message-Id" ,msgid))))
    (and (pair? res)
         (null? (cdr res))
         (car res))))

(define (imap-input-handler src dummy-filter)
  (let* ((host (or (conf-get src 'host)
                   (conf-get src 'smtp-host "localhost")))
         (port (conf-get src 'port))
         (ssl? (conf-get src 'ssl?
                         (or (eq? 'imaps (conf-get src 'protocol))
                             (eqv? port 993))))
         (port (or port (if ssl? 993 220)))
         (debug? (conf-get src 'debug?))
         (delete? (conf-get src 'delete?))
         (delete-after (conf-get src 'delete-after))
         (mbox-list (conf-get-list src 'mailboxes '("INBOX")))
         (mbox-include-list (map ->string (remove pair? mbox-list)))
         (mbox-literal-list
          (filter (lambda (x) (not (string-search "[%*]" x)))
                  mbox-include-list))
         (mbox-patterns-list
          (remove (lambda (x) (not (string-search "[%*]" x)))
                  mbox-include-list))
         (mbox-exclude-list
          (map
           ->string
           (append-map
            cdr
            (filter (lambda (x) (and (pair? x) (eq? 'except (car x))))
                    mbox-list))))
         (mbox-exclude-pattern
          (and (pair? mbox-exclude-list)
               (string-append
                "^(("
                (string-intersperse
                 (map mbox-pattern->regexp mbox-exclude-list)
                 ")|(")
                "))$")))
         (mbox-exclude-regexp
          (and mbox-exclude-pattern (regexp mbox-exclude-pattern)))
         (user (or (conf-get src 'username) (current-user-name)))
         (pass
          (or (conf-get src 'password)
              (read-password
               (sprintf "Password for ~A@~A: " user host)
               (lambda (str)
                 (let ((res (imap-connect
                             host user str debug? port #f ssl?)))
                   (cond
                    (res (imap-disconnect res) #t)
                    (else #f))))))))
    (receive (filt-imap filt-man) (get-imap-filter-function src)
      (lambda (kons knil)
        (let* ((imap (imap-connect host user pass debug? port #f ssl?))
               (no-history? (conf-get src 'no-history?))
               (src (if no-history? src (conf-restore src)))
               (history (conf-get-alist src 'history))
               (initial-mboxes
                (append mbox-literal-list
                        (remove
                         (lambda (x)
                           (and mbox-exclude-regexp
                                (string-match mbox-exclude-regexp x))) 
                         (if (not imap)
                             '()
                             (append-map
                              (lambda (x)
                                (map caddr (imap-list imap "" x))) 
                              mbox-patterns-list)))))
               (last-msgid #f))
          (cond
           ((not imap)
            (log-error "couldn't connect to imap~A://~A@~A"
                       (if ssl? "s" "") user host))
           (else
            (log-notice "polling imap~A://~A@~A ~S"
                        (if ssl? "s" "") user host initial-mboxes)
            (if (conf-get src 'no-run?)
                (begin
                  (imap-disconnect imap)
                  knil)
                (let lp ((mboxes initial-mboxes)
                         (knil knil)
                         (statuses '()))
                  (cond
                   ((null? mboxes)
                    (imap-close imap)
                    (imap-disconnect imap)
                    (if (not no-history?)
                        (conf-save
                         src
                         `((version ,*program-version*)
                           (last-fetch ,(current-seconds))
                           (history
                            ,@(append statuses
                                      (remove (lambda (x)
                                                (assoc (car x)
                                                       statuses))
                                              history))))))
                    knil)
                   (else
                    (cond
                     ((not (imap-select imap (car mboxes)))
                      (log-warn "couldn't select mailbox: ~S" (car mboxes))
                      (lp (cdr mboxes) knil statuses))
                     (else
                      ;; fetch any new messages
                      (let* ((hist (cond ((assoc (car mboxes) history) => cdr)
                                         (else '())))
                             (status
                              (imap-status->alist imap
                                                  (car mboxes)
                                                  '(UIDNEXT UIDVALIDITY)))
                             (same-uidvalidity?
                              (equal? (cond ((assq 'uidvalidity hist)
                                             => cadr)
                                            (else -1))
                                      (cond ((assq 'UIDVALIDITY status)
                                             => cdr)
                                            (else -2))))
                             (range
                              (cond
                               ((and (or same-uidvalidity?
                                         (not (assq 'uidvalidity hist)))
                                     (equal? (cond ((assq 'uidnext hist)
                                                    => cadr)
                                                   (else -1))
                                             (cond ((assq 'UIDNEXT status)
                                                    => cdr)
                                                   (else -2))))
                                ;; no new messages since last fetch
                                '())
                               ((cond ((and same-uidvalidity?
                                            (assq 'uidnext hist))
                                       => cadr)
                                      (else #f))
                                => (lambda (uidnext)
                                     ;; try fetching by uid range
                                     (log-notice "fetching ~A ~S:*"
                                                 (car mboxes) uidnext)
                                     (imap-search/uid
                                      imap
                                      `(or (,uidnext . *) unseen))))
                               ((cond ((assq 'last-msgid hist)
                                       => (lambda (msgid)
                                            (imap-message-id->uid imap msgid)))
                                      (else #f))
                                ;; uidvalidity changed, find the new
                                ;; value from the message id
                                (lambda (uidlast)
                                  (log-notice
                                   "uidvalidity changed, fetching ~A ~S:*"
                                   (car mboxes) (+ 1 uidlast))
                                  (imap-search/uid
                                   imap
                                   `(or (,(+ 1 uidlast) . *) unseen))))
                               (else
                                ;; no info for last fetch, grab everything
                                (if (not no-history?)
                                    (log-notice
                                     "initial fetch of ~A (be patient)"
                                     (car mboxes)))
                                '(1 . *))))
                             ;; include any imap-level filtering
                             (range (if (pair? filt-imap)
                                        (append filt-imap (list range))
                                        range))
                             ;; collect the results
                             (res
                              (if (null? range)
                                  knil
                                  (imap-fetch-fold/uid
                                   imap
                                   range
                                   (lambda (id text acc)
                                     (let ((headers
                                            (call-with-input-string
                                             text
                                             mime-headers->list)))
                                       (set! last-msgid
                                             (string-trim-both
                                              (mime-ref headers "Message-Id")))
                                       (if (filt-man text headers)
                                           (cons (cons id (car acc))
                                                 (kons text headers (cdr acc)))
                                           acc)))
                                   (cons '() knil)))))
                        ;; note the range
                        (log-debug "range: ~S" range)
                        ;; ... maybe delete them
                        (cond
                         (delete-after
                          (let* ((offset (* delete-after 60 60 24))
                                 (dt (- (current-seconds) offset)))
                            (imap-store imap
                                        `(SEEN BEFORE ,dt)
                                        "+FLAGS.SILENT"
                                        '("\\DELETED"))))
                         (delete?
                          (if (not (null? range))
                              (imap-store/uid
                               imap range "+FLAGS.SILENT" '("\\DELETED")))))
                        ;; flag them as seen if not deleted
                        (if (and (not delete?)
                                 (not (conf-get src 'no-flag-seen?)))
                            (if (not (null? range))
                                (imap-store/uid
                                 imap range "+FLAGS.SILENT" '("\\SEEN"))))
                        ;; ... save our results and proceed to the next mailbox
                        (let* ((new-status
                                (imap-status->alist
                                 imap (car mboxes) '(MESSAGES)))
                               (new-hist
                                `(,(car mboxes)
                                  (fetch-time ,(current-seconds))
                                  (messages
                                   ,(cond ((assq 'MESSAGES new-status) => cdr)
                                          (else #f)))
                                  (uidnext
                                   ,(cond ((assq 'UIDNEXT status) => cdr)
                                          (else #f)))
                                  (last-msgid ,last-msgid)
                                  (uidvalidity
                                   ,(cond ((assq 'UIDVALIDITY status) => cdr)
                                          (else #f))))))
                          (log-debug "saved history: ~S" new-hist)
                          (lp (cdr mboxes) res (cons new-hist statuses))
                          )))))))))))))))

(define (get-input-handler src)
  (let ((filter (get-filter-function src)))
    (case (conf-get src 'protocol)
      ((port) (port-input-handler src filter))
      ((file mbox mh maildir) (file-input-handler src filter))
      ((test) (test-input-handler src filter))
      ((null) (null-input-handler src filter))
      ((pipe) (pipe-input-handler src filter))
      ((pop pop3 pop3s) (pop3-input-handler src filter))
      ((imap imaps) (imap-input-handler src filter))
      (else (die "invalid mail protocol: ~S" (conf-get src 'protocol))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; output handlers

(define (default-output-handler dest)
  (lambda (text headers)
    (cond
     ((conf-get dest 'default)
      => (lambda (default)
           ((get-output-handler
             (if (symbol? default)
                 (get-named-mbox
                  (conf-extend `((default #f)) dest)
                  default)
                 default))
            text headers)))
     (else
      (send-mail To: (current-user-name) Source: text)))))

(define (port-output-handler dest)
  (lambda (text headers)
    (condition-case (begin (display text (conf-get dest 'port)) #t)
      (exn () #f))))

(define (file-output-handler dest)
  (lambda (text headers)
    (let* ((file (conf-get dest 'file))
           (format (if (file-exists? file)
                       #f
                       (conf-get dest 'default-format))))
      (mail-archive-add file text format headers))))

(define (maildir-output-handler dest)
  (lambda (text headers)
    (mail-archive-add (conf-get dest 'file) text 'maildir)))

(define (mh-output-handler dest)
  (lambda (text headers)
    (mail-archive-add (conf-get dest 'file) text 'mh)))

(define (mbox-output-handler dest)
  (lambda (text headers)
    (mail-archive-add (conf-get dest 'file) text 'mbox)))

(define (pipe-output-handler dest)
  (lambda (text headers)
    (receive (in out pid) (process (or (conf-get dest 'command) "procmail"))
      (display text out)
      (close-output-port out)
      (receive (pid ok? status) (process-wait pid)
        (or (and ok? (zero? status))
            ;; if the procmail process returns non-0, fallback on the
            ;; user-specified default, or local smtp if no default
            (cond
             ((conf-get-default dest)
              => (lambda (default)
                   ((get-output-handler default) text headers)))
             (else
              (send-mail To: (current-user-name) Source: text))))))))

(define (null-output-handler src)
  (lambda (text headers) #t))

(define (smtp-output-handler dest)
  (let* ((addrs (conf-get-list dest 'address))
         (addrs (if (pair? addrs)
                    addrs
                    (list (conf-get dest 'username (current-user-name)))))
         (host (or (conf-get dest 'host)
                   (conf-get dest 'smtp-host "localhost"))))
    (define (local-address? addr)
      (let ((domain (domain-part addr)))
        (or (not domain) (string-ci=? domain "localhost"))))
    (cond
     ((and (not (conf-get dest 'allow-relay?))
           (not (every local-address? addrs)))
      (log-error "you must specify --allow-relay to forward to remote hosts")
      (lambda (text headers) #f))
     (else
      (lambda (text headers)
        (send-mail Host: host To: addrs Source: text))))))

(define (imap-output-handler dest)
  (let* ((host (or (conf-get dest 'host)
                   (conf-get dest 'smtp-host "localhost")))
         (port (conf-get dest 'port))
         (ssl? (conf-get dest 'ssl?
                         (or (eq? 'imaps (conf-get dest 'protocol))
                             (eqv? port 993))))
         (port (or port (if ssl? 993 220)))
         (debug? (conf-get dest 'debug?))
         (delete? (conf-get dest 'delete?))
         (delete-after (conf-get dest 'delete-after))
         (mbox0 (conf-get dest 'mailbox))
         (mbox (if (and mbox0 (not (equal? "" mbox0))) mbox0 'INBOX))
         (user (or (conf-get dest 'username) (current-user-name)))
         (pass
          (or (conf-get dest 'password)
              (read-password
               (sprintf "Password for ~A@~A: " user host)
               (lambda (str)
                 (let ((res (imap-connect
                             host user str debug? port #f ssl?)))
                   (cond
                    (res (imap-disconnect res) #t)
                    (else #f))))))))
    (lambda (text headers)
      (let ((imap (imap-connect host user pass debug? port #f ssl?)))
        (let ((res (and imap (imap-append imap mbox text))))
          (imap-disconnect imap)
          res)))))

(define (hato-output-handler dest)
  (let* ((file (or (conf-get dest 'file) (expand-path "~/.hato/filter")))
         (proc (condition-case (load-user-filter file)
                 (exn ()
                      (die "error loading filter: ~A"
                           (exception-message exn)))))
         (load-time (current-seconds)))
    (lambda (text headers)
      (cond
       ((> (condition-case (file-modification-time file) (exn () 0))
           load-time)
        (log-notice "reloading filter from ~S" file)
        (let ((new-proc (condition-case (load-user-filter file)
                          (exn ()
                               (log-error "error loading filter")
                               (log-error&call-chain 'error exn)
                               #f))))
          (cond
           (new-proc
            (set! proc new-proc)
            (set! load-time (current-seconds)))))))
      (let* ((addr (parse-mail-address
                    (or (mime-ref headers "%From")
                        (mime-ref headers "From")
                        (mime-ref headers "Sender"))))
             (addr (if (pair? addr) (car addr) "<unknown>"))
             (msgid (mime-ref headers "Message-Id"))
             (mail (make-mail text headers)))
        (condition-case
            (let lp ((res
                      (with-timeout
                       (conf-get dest 'filter-timeout 60)
                       (lambda ()
                         (with-output-to-port current-log-port
                           (lambda ()
                             (safe-apply-as-user
                              (lambda () (proc addr mail))
                              (current-user-id))))))))
              (cond
               ((and (pair? res) (memq (car res) '(discard reject refuse)))
                (log-notice "discarding ~A from ~A" msgid addr)
                #t) ; silently discard
               ((and (pair? res) (eq? 'relay (car res)))
                (log-notice "forwarding ~A from ~A to ~A" msgid addr (cdr res))
                ((smtp-output-handler
                  (conf-extend `((address ,@(cdr res)))
                               dest))
                 text headers))
               ((and (pair? res) (eq? 'file (car res)))
                (let ((path (expand-user-path
                             (current-user-name)
                             (cadr res)
                             get-user-home
                             (conf-get dest 'maildir "~/Mail/"))))
                  (log-notice "adding ~A from ~A to ~A" msgid addr path)
                  (mail-archive-add path text (conf-get dest 'default-format))))
               ((and (pair? res) (string? (car res)))
                (every lp res))
               ((string? res)
                (cond
                 ((string-index res #\@) (lp (list 'relay res)))
                 (else (lp (list 'file res)))))
               ((or (not res) (eq? res (if #f #f)))
                ((default-output-handler dest) text headers))
               (else
                (log-warn "unknown filter result: ~S" res)
                ((default-output-handler dest) text headers)
                #f)))
          (exn ()
               (log-error "error in user filter")
               (log-error&call-chain 'error exn)
               ((default-output-handler dest) text headers)
               #f))))))

(define (get-output-handler dest)
  (case (conf-get dest 'protocol)
    ((port)        (port-output-handler dest))
    ((file)        (file-output-handler dest))
    ((maildir)     (maildir-output-handler dest))
    ((mh)          (mh-output-handler dest))
    ((mbox)        (mbox-output-handler dest))
    ((pipe)        (pipe-output-handler dest))
    ((smtp)        (smtp-output-handler dest))
    ((null)        (null-output-handler dest))
    ((imap imaps)  (imap-output-handler dest))
    ((hato)        (hato-output-handler dest))
    (else          (die "incomplete mail destination: ~S" dest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-filter-function conf)
  (let* ((filters (conf-multi conf 'filter))
         (filter-fn (compile-filter (cons 'and filters)))
         (removes (conf-multi conf 'remove))
         (remove-fn (compile-filter (cons 'or filters))))
    (cond
     ((and (null? filters) (null? removes))
      (lambda (text headers) #t))
     ((and (pair? filters) (null? removes))
      (lambda (text headers)
        (filter-fn text headers)))
     ((and (null? filters) (pair? removes))
      (lambda (text headers)
        (not (remove-fn text headers))))
     ((and (pair? filters) (pair? removes))
      (lambda (text headers)
        (and (filter-fn text headers)
             (not (remove-fn text headers)))))
     )))

(define (imap-search-keyword? x)
  (memq x '(answered deleted draft flagged new old recent seen
            unanswered undeleted undraft unflagged unseen all)))

(define (imap-search-function? x)
  (memq x '(bcc before body cc from header keyword larger on
            sentbefore senton sentsince since smaller
            subject text to uid)))

(define (imap-search-filter? x)
  (or (imap-search-keyword? (if (pair? x) (car x) x))
      (and (pair? x) (imap-search-function? (car x)))))

(define (adjust-imap-dates x)
  (if (not (pair? x))
      x
      (case (car x)
        ((before on since sentbefore senton sentsince)
         (cons (car x)
               (cons (cond
                      ((string? (cadr x))
                       (or (string->number (cadr x))
                           (parse-date (cadr x))
                           (cadr x)))
                      ((symbol? (cadr x))
                       (or (parse-date (symbol->string (cadr x)))
                           (cadr x)))
                      (else (cadr x)))
                     (adjust-imap-dates (cddr x)))))
        (else
         (cons (adjust-imap-dates (car x))
               (adjust-imap-dates (cdr x)))))))

(define (get-imap-filter-function conf)
  (let* ((filters (conf-multi conf 'filter))
         (removes (conf-multi conf 'remove)))
    (receive (filt-imap filt-man) (partition imap-search-filter? filters)
      (receive (rem-imap rem-man) (partition imap-search-filter? removes)
          (values (append (map adjust-imap-dates filt-imap)
                          (map (lambda (x) (list 'not x))
                               (map adjust-imap-dates rem-imap)))
                  (get-filter-function `(((filter ,@filt-man)
                                          (remove ,@rem-man)))))))))

(define (compile-filter x)
  (cond
   ((pair? x)
    (case (car x)
      ((not)
       (let ((f (compile-filter (cadr x))))
         (lambda (text headers)
           (not (f text headers)))))
      ((and)
       (case (length (cdr x))
         ((0) (lambda (text headers) #t))
         ((1) (compile-filter (cadr x)))
         (else
          (let ((head (compile-filter (car x)))
                (tail (compile-filter (cons 'and (cddr x)))))
            (lambda (text headers)
              (and (head text headers)
                   (tail text headers)))))))
      ((or)
       (case (length (cdr x))
         ((0) (lambda (text headers) #f))
         ((1) (compile-filter (cadr x)))
         (else
          (let ((head (compile-filter (car x)))
                (tail (compile-filter (cons 'or (cddr x)))))
            (lambda (text headers)
              (or (head text headers)
                  (tail text headers)))))))
      ((larger smaller)
       (let ((size (string->number (->string (cadr x))))
             (op (if (eq? 'larger (car x)) > <)))
         (lambda (text headers)
           (op (string-length text) size))))
      ((before on since sentbefore senton sentsince)
       (let ((cmp-dt
              (cond ((number? (cadr x)) (cadr x))
                    ((vector? (cadr x)) (local-time->seconds (cadr x)))
                    ((string->number (->string (cadr x))))
                    ((parse-date (->string (cadr x))))
                    (else (error "compile-filter: invalid date" (cadr x))))))
         (lambda (text headers)
           (let ((dt (case (car x)
                       ((before on since)
                        (cond
                         ((or (mime-ref headers "%from")
                              (mime-ref headers "date"))
                          => parse-date)
                         (else #f)))
                       (else
                        (mime-sent headers)))))
             (and dt
                  ((case (car x)
                     ((before sentbefore) date<=?)
                     ((since sentsince) date>=?)
                     (else date=?))
                   dt cmp-dt))))))
      ((body)
       (let ((keywords (cdr x)))
         (lambda (text headers)
           (any (lambda (pat) (string-contains-ci text pat)) keywords))))
      ((header)
       (if (not (= 3 (length x)))
           (error "compile-filter: header filter requires two arguments" x)
           (let ((str (cadr x))
                 (pat (caddr x)))
             (lambda (text headers)
               (let ((val (mime-ref headers str)))
                 (and val (string-contains-ci val pat)))))))
      (else
       (if (not (= 2 (length x)))
           (error "implicit header filter requires exactly one argument" x)
           (let ((str (symbol->string (car x)))
                 (pat (cadr x)))
             (lambda (text headers)
               (let ((val (mime-ref headers str)))
                 (and val (string-contains-ci val pat)))))))))
   ((symbol? x)
    (if (imap-search-keyword? x)
        ;; allow these for the imap filter to optionally handle - they
        ;; have no meaning in general so we just assume they're all
        ;; true
        (lambda (text headers) #t)
        ;; other symbols look for the existence of a given header
        (let ((str (symbol->string x)))
          (lambda (text headers)
            (mime-ref headers str)))))
   (else
    (error "compile-filter: unknown filter" x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user filters

(define (file->sexp-list file)
  (call-with-input-file file
    (lambda (p)
      (let lp ((ls '()))
        (let ((o (read p)))
          (if (eof-object? o)
              (reverse ls)
              (lp (cons o ls))))))))

(define (load-user-filter file)
  (let ((sexp-ls (parameterize ((case-sensitive #f))
                   (file->sexp-list file))))
    (safe-eval-as-user
     `(lambda (addr mail)
        (current-address addr)
        (current-mail mail)
        (call-with-current-continuation
         (lambda (return)
           (letrec
               ((reject
                 (lambda o (return (cons 'reject o))))
                (discard
                 (lambda o (return (cons 'discard o))))
                (refuse
                 (lambda o (return (cons 'refuse o)))))
             ,@sexp-ls))))
     '(regex hato-filter-env)
     (current-user-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remember the last time we fetched something

(define (conf-restore cfg)
  (let ((uri (source->canonical-uri cfg)))
    (if (not uri)
        cfg
        (let ((res
               (let ((path (expand-path "~/.hato/fetch/history.db")))
                 (cond
                  ((not (db-file? path))
                   (if (file-exists? path)
                       (log-warn "not a database file: ~S" path))
                   #f)
                  (else
                   (let* ((db (open-db path))
                          (res (db-ref db uri)))
                     (close-db db)
                     (condition-case
                         (call-with-input-string res read)
                       (exn ()
                            (log-warn "couldn't read database data: ~S" res)
                            #f))))))))
          (if (pair? res)
              (conf-extend res cfg)
              cfg)))))

(define (conf-save cfg data)
  (let ((uri (source->canonical-uri cfg)))
    (if uri
        (let ((path (expand-path "~/.hato/fetch/history.db")))
          (if (not (file-exists? (expand-path "~/.hato")))
              (create-directory (expand-path "~/.hato")))
          (if (not (file-exists? (expand-path "~/.hato/fetch")))
              (create-directory (expand-path "~/.hato/fetch")))
          (let ((db (open-db path)))
            (db-set! db uri (call-with-output-string
                              (lambda (out) (write data out))))
            (close-db db))))))

(define (adjust-alias config src)
  (if (eq? 'alias (conf-get src 'protocol))
      (adjust-alias config (get-named-mbox config (conf-get src 'alias)))
      src))

(define (get-named-mbox config name)
  (let* ((name (if (string? name)
                   (string->symbol (string-downcase name))
                   name))
         (sources (list (conf-get-alist config 'sources '())))
         (res (conf-get-list sources name #f)))
    (if res
        (conf-extend res config)
        (die "unknown mailbox: ~S" name))))

(define (conf-get-default config)
  (let ((default (conf-get config 'default)))
    (let ((default
            (cond
             ((symbol? default)
              (get-named-mbox (conf-extend `((default #f)) config)
                              default))
             ((pair? default)
              (conf-extend `((default #f)) default))
             (else
              #f))))
      (and default
           (get-output-handler default)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

(define (show-help . o)
  (display
"usage: hato-fetch [options] mailboxes ...

  hato-fetch                            # run default fetch
  hato-fetch mbox1                      # fetch from mbox1
  hato-fetch mbox1 mbox2                # move from mbox1 to mbox2
  hato-fetch mbox1 ... mboxN-1 mboxN    # move multiple source into mboxN

Recognized mailboxes:

  /path/to/file        (mbox, mh or maildir recognized)
  file:/path/to/file   same as above
  pipe:command         use piped I/O to/from a command
  alias:name           a named mbox from the config file
  :name                same as above
  imap[s]://user@host/mailbox?search#messages
  null:                the bit bucket
  -                    stdin/stdout

Input only mailboxes:

  pop:user@host        pull from a pop server
  test:[subject]       generate a timestamped test message

Output only mboxes:

  mbox:/path/to/file   these three are all equivalent to
  mh:/path/to/file         file:/path/to/file but indicate the
  maildir:/path/to/file    preferred format for new mail archives

  smtp:user@host       forward the mail
  user@host            same as above
  smtp:                delivers to local user by default

Options:

  -h, --help             print this message and exit
  -V, --version          print version and exit
  -c, --config=FILE      specify config file (default ~/.hato/fetch/conf)
      --no-config        don't use any config file
  -n, --no-run           trial run, verify servers but don't fetch
  -d, --daemon           run the command periodically as a daemon
  -k, --kill             kill running daemon
      --fetch            force a fetch right now
  -i, --interval=N       interval to use in seconds (default 60)
      --delete           delete fetched messages (i.e. mv instead of cp)
      --delete-after=N   delete old messages after N days
  -f, --filter key[=val] fetch only messages where the key (header) matches
  -r, --remove key[=val] exclude messages where the key (header) matches
      --cc=MBOX          CC to an extra destination mbox
      --allow-relay      enable relaying to external mail addresses
")
  (exit))

(define (show-version . o)
  (display *program-name*)
  (display " ")
  (display *program-version*)
  (newline)
  (exit))

(define (get-all-inputs mboxes config conf-prefix interval)
  (let ((sources (conf-get-alist config 'sources)))
    (map (lambda (x)
           (let* ((conf (conf-extend conf-prefix
                                     (adjust-alias config
                                                   (conf-extend x config))))
                  (interval (or (conf-get conf 'interval) interval 60)))
             (if (not (and (integer? interval) (positive? interval)))
                 (die "invalid interval ~S for ~S" interval x))
             (cons interval (get-input-handler conf))))
         (cond
          ((null? mboxes)
           (map
            (lambda (x)
              (if (symbol? x)
                  `((protocol alias) (alias ,x))
                  x))
            (or (conf-get-list config 'auto #f)
                (if (pair? sources)
                    sources
                    (die "no default sources to fetch from")))))
          ((null? (cdr mboxes))
           (map parse-input-uri mboxes))
          (else
           (map parse-input-uri (drop-right mboxes 1)))))))

(define (get-all-outputs mboxes cc config conf-prefix)
  (map (lambda (x)
         (let ((conf (conf-extend conf-prefix
                                  (adjust-alias config
                                                (conf-extend x config)))))
           (get-output-handler conf)))
       (cons (cond
              ((> (length mboxes) 1)
               (parse-output-uri (last mboxes)))
              ((file-exists? (expand-path "~/.hato/filter"))
               (log-notice "using filter output")
               `((protocol hato) (file #f)))
              ((conf-get config 'destination)
               => (lambda (dest)
                    (cond
                     ((string? dest) (parse-output-uri dest))
                     ((symbol? dest) `((protocol alias) (alias ,dest)))
                     ((pair? dest) dest)
                     (else (die "invalid destination ~S" dest)))))
              (else
               '((protocol smtp) (host "localhost"))))
             (map parse-output-uri (reverse cc)))))

(define (run-fetch from to-ls)
  (condition-case
      (from (lambda (text headers acc)
              (and acc
                   (every (lambda (f) (f text headers)) to-ls)))
            #t)
    (exn () (log-error&call-chain 'error exn))))

(define (main args)
  (let-args args
      ((help "help|h" => show-help)
       (version "version|V" => show-version)
       (conf-dir "base-dir|base=s" (expand-path "~/.hato/fetch"))
       (rcfile "config|c=s" (expand-path (string-append conf-dir "/config")))
       (norc? "no-config")
       (no-run? "no-run|n")
       (daemon? "daemon|d")
       (kill? "kill|k")
       (fetch? "fetch")
       (interval "interval|i=i")
       (delete? "delete")
       (delete-after "delete-after=i")
       (allow-relay? "allow-relay")
       (default "default=s")
       (filters "filter|f=s*")
       (removes "remove|r=s*")
       (cc "cc=s*")
       (else (opt rest cont) (die "invalid option ~S" opt))
       . mboxes)

    (define (run running?)
      (let* ((config
              (conf-extend
               `(,@(if default `((default ,default)) '())
                 ,@(if (pair? filters)
                       `((filter ,@(map parse-filter filters)))
                       '())
                 ,@(if (pair? removes)
                       `((remove ,@(map parse-filter removes)))
                       '()))
               (if (and (not norc?) (file-exists? rcfile))
                   (if (zero? (bitwise-and (file-permissions rcfile) #o77))
                       (conf-load rcfile)
                       (die "config file is readable by others: ~A" rcfile))
                   '())))
             (load-time (current-seconds))
             (daemon? (or daemon? (conf-get config 'daemon?)))
             (maildir (conf-get config 'maildir (expand-path "~/Mail/")))
             (sources (list (conf-get-alist config 'sources '())))
             (pid-file (conf-get config 'pid-file
                                 (expand-path
                                  (string-append conf-dir "/fetch.pid"))))
             (log-file (conf-get config 'log-file
                                 (expand-path
                                  (string-append conf-dir "/fetch.log"))))
             (fifo-file (conf-get config 'fifo-file
                                  (expand-path
                                   (string-append conf-dir "/fetch.fifo")))))

        ;; setup dirs and logging
        (if (not (directory? conf-dir))
            (create-directory* conf-dir))
        (if (and log-file (or daemon? kill? fetch?))
            (begin
              (log-close)
              (log-open log-file)))
        (let ((log-level (or (conf-get config 'log-level)
                             (if (conf-get config 'debug?) 7 6))))
          (if (and (integer? log-level) (<= 1 log-level 10))
              (set! current-log-level log-level)))
        (log-notice "config: ~S" config)

        ;; warn users about bad config entries
        (conf-verify
         config
         (let ((base-specs
                `((address string)
                  (alias symbol)
                  (auto (list symbol))
                  (command string)
                  (daemon? boolean)
                  (debug? boolean)
                  (default)
                  (default-format (member mh mbox maildir))
                  (delete-after integer)
                  (delete? boolean)
                  (destination)
                  (fifo-file filename)
                  (file filename)
                  (filter (list (list symbol object)))
                  (from string)
                  (host string)
                  (interval integer)
                  (log-file filename)
                  (log-level filename)
                  (mailboxes
                   (list (or string
                             symbol
                             (cons 'except (list (or string symbol))))))
                  (maildir dirname)
                  (no-flag-seen? boolean)
                  (no-history? boolean)
                  (password string)
                  (pid-file filename)
                  (port integer)
                  (protocol symbol)
                  (remove (list (list symbol object)))
                  (smtp-host string)
                  (ssl? boolean)
                  (to string)
                  (username string)
                  )))
           `((sources (list (cons symbol (alist ,@base-specs)))) ,@base-specs))
         (lambda (str) (log-warn "~A" str))
         )

        (cond

         (kill?
          (daemon-kill pid-file
                       name: "hato-fetch"
                       notifier: (lambda (fmt . args)
                                   (if (>= current-log-level 5)
                                       (apply log-format 'notify fmt args)))
                       warner: (lambda (fmt . args)
                                 (if (>= current-log-level 4)
                                     (apply log-format 'warn fmt args)))))

         (fetch?
          (let ((pid (condition-case (call-with-input-file pid-file read)
                       (exn () #f))))
            (if (and (number? pid) (running-process-id? pid "hato-fetch"))
                (let ((fd (file-open fifo-file
                                     (bitwise-ior open/rdwr open/nonblock))))
                  (file-write fd "f" 1)
                  (file-close fd))
                (log-error "hato-fetch isn't running"))))

         (else

          ;; bind src and dest before daemonizing in case we need to
          ;; read passwords from the command-line
          (let* ((conf-prefix
                  `((allow-relay? ,allow-relay?)
                    (no-run? ,no-run?)
                    ,@(if delete? `((delete? #t)) '())
                    ,@(if (number? delete-after)
                          `((delete-after ,delete-after))
                          '())))
                 (src
                  (sort
                   (get-all-inputs mboxes config conf-prefix interval)
                   (lambda (a b) (< (car a) (car b)))))
                 (intervals
                  (unique/sorted (map car src)))
                 (full-interval (apply lcm intervals))
                 (interval-steps
                  `(0 
                    ,@(expand-interval-steps intervals full-interval)
                    ,full-interval))
                 (dest
                  (get-all-outputs mboxes cc config conf-prefix))
                 (fetch-fifo
                  (and
                   daemon?
                   (cond
                    ((file-exists? fifo-file)
                     (or (fifo? fifo-file)
                         (begin
                           (log-error "fifo ~S not really a fifo" fifo-file)
                           #f)))
                    (else
                     (create-fifo fifo-file)))
                   (condition-case
                       (file-open fifo-file
                                  (bitwise-ior open/rdonly open/nonblock))
                     (exn ()
                          (log-error "couldn't open fifo ~S" fifo-file)
                          #f)))))

            (cond
             (daemon?
              (cond
               ((not running?)
                (daemonize name: "hato-fetch" pid-file: pid-file)
                (if (and (output-port? current-log-port)
                         (port-open? current-log-port))
                    (current-error-port current-log-port))))
              (log-notice "starting polling at ~S second intervals"
                          intervals)
              (gc)

              ;; fetch in possibly variable intervals
              (let lp ((i interval-steps))
                (cond
                 ((> (condition-case (file-modification-time rcfile)
                       (exn () 0))
                     load-time)
                  (log-notice "conf file modified, reloading")
                  (file-close fetch-fifo)
                  (run #t))
                 (else
                  (let ((start (current-seconds)))
                    (for-each
                     (lambda (x)
                       (if (zero? (remainder (car i) (car x)))
                           (run-fetch (cdr x) dest)))
                     src)
                    (gc)
                    (let* ((interval (- (cadr i) (car i)))
                           (rem (- interval (- (current-seconds) start))))
                      (cond
                       ((positive? rem)
                        (flush-output current-log-port)
                        (if fetch-fifo
                            (fifo-clear fetch-fifo rem)
                            (sleep (inexact->exact (round rem))))
                        ))
                      (lp (if (null? (cddr i))
                              interval-steps
                              (cdr i)))))))))

             (else
              (for-each (lambda (x) (run-fetch (cdr x) dest)) src))))))))

    ;; start
    (run #f)))

)

(import hato-fetch)

(main (command-line-arguments))

