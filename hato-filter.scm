;;;; hato-filter.scm -- user-level mail filters
;;
;; Copyright (c) 2005-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module hato-filter
 (mail? make-mail
  mail-text set-mail-text!
  mail-headers set-mail-headers!
  mail-stats set-mail-stats!
  hato-user-env
  hato-user-env-definitions
  hato-sys-env-definitions
  )

(import scheme chicken)

(use user-env hato-prob hato-archive hato-db hato-mime hato-smtp
     domain-keys srfi-1 srfi-13 posix regex sandbox)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <mail>
  (%make-mail text headers stats)
  mail?
  (text mail-text set-mail-text!)
  (headers mail-headers set-mail-headers!)
  (stats mail-stats set-mail-stats!))

(define (make-mail . o)
  (let-optionals* o ((text "") (headers #f) (stats #f))
    (%make-mail text headers stats)))

(define-macro (safe-environment-extend! env . vars)
  (let ((e (gensym)))
    `(let ((,e ,env))
       ,@(map (lambda (v)
                (let ((name (if (pair? v) (car v) v))
                      (val (if (pair? v) (cadr v) v)))
                  `(safe-environment-set! ,e ',name ,val)))
              vars))))

(define hato-user-env-definitions
  '(begin
     (define open-db
       (let ((%open-db %open-db))
         (lambda (file . o)
           (let* ((read-only? (and (pair? o) (car o)))
                  (check (if read-only? file-readable? file-writeable?))
                  (ok? (check file)))
             (if (eq? ok? 'new)
                 (touch-file file (current-user-id) (current-group-id)))
             (if ok?
                 (%open-db file read-only?)
                 (error (if read-only?
                            "file not readable: "
                            "file not writeable: ")
                        file))))))
     (define (default-folder)
       (hato-default-user-folder (current-user-id)))
     (define (spam-db-file)
       (expand-user-dir (current-user-name) "~/.hato/spam.db"))
     (define (headers)
       (or (mail-headers (current-mail))
           (let ((h (with-input-from-string
                        (mail-text (current-mail))
                      mime-headers->list)))
             (set-mail-headers! (current-mail) h)
             h)))
     (define (header name . o)
       (apply mime-ref (headers) name o))
     (define (to)
       (map car (parse-mail-address-list (header "To" ""))))
     (define (cc)
       (map car (parse-mail-address-list (header "Cc" ""))))
     (define (bcc) ;; shouldn't ever see this, but just in case
       (map car (parse-mail-address-list (header "Bcc" ""))))
     (define (to/cc)
       (append (to) (cc) (bcc)))
     (define (from)
       (car (parse-mail-address (or (header "%From") (header "From")))))
     (define (match-addrs real-addrs tests)
       (any (lambda (test-addr)
              (member
               (if (eqv? #\@ (string-ref test-addr 0))
                   (substring test-addr 1)
                   test-addr)
               (filter
                string?
                (let ((at (string-index test-addr #\@)))
                  (cond
                   ((not at) (map local-part real-addrs))
                   ((zero? at) (map domain-part real-addrs))
                   (else real-addrs))))
               string-ci=?))
            tests))
     (define (to? . addrs)
       (match-addrs (to) addrs))
     (define (cc? . addrs)
       (match-addrs (cc) addrs))
     (define (to/cc? . addrs)
       (match-addrs (to/cc) addrs))
     (define (from? . addrs)
       (match-addrs (list (from)) addrs))
     (define (date)
       (header "Date"))
     (define (subject)
       (header "Subject"))
     (define (mailer)
       (or (header "User-Agent") (header "X-Mailer")))
     (define (message-id)
       (header "Message-Id"))
     (define (size)
       (string-length (mail-text (current-mail))))
     (define domain-key-verify
       (let ((%domain-key-verify %domain-key-verify))
         (lambda o
           (%domain-key-verify
            (if (pair? o) (car o) (mail-text (current-mail)))))))
     (define get-stats
       (let ((feature-fold feature-fold))
         (lambda keys
           (or (mail-stats (current-mail))
               (with-input-from-string (mail-text (current-mail))
                 (lambda ()
                   (let ((stats
                          (apply feature-fold
                                 '()
                                 (string->keyword "db-file")
                                 (spam-db-file)
                                 keys)))
                     (set-mail-stats! (current-mail) stats)
                     stats)))))))
     (define (spam-probability)
       (mstats-prob (get-stats)))
     (define (is-spam? . o)
       (> (spam-probability) (if (pair? o) (car o) 0.6)))
     (define (Urls)   (mstats-urls (get-stats)))
     (define (Emails) (mstats-emails (get-stats)))
     (define (Ips)    (mstats-ips (get-stats)))
     (define (message-id-file)
       (expand-user-dir (current-user-name) "~/.hato/msgid.db"))
     (define (is-duplicate?)
       (let ((id (message-id))
             (db (open-db (message-id-file))))
         (and db
              (let ((res (db-ref db id)))
                (if (not res)
                    (db-set! db id (current-seconds)))
                (close-db db)
                res))))
     (define (white-list-file)
       (expand-user-dir (current-user-name) "~/.hato/white.db"))
     (define (black-list-file)
       (expand-user-dir (current-user-name) "~/.hato/black.db"))
     (define (in-list? file . o)
       (let ((str (if (pair? o)
                      (car o)
                      (car (parse-mail-address (from)))))
             (db (open-db file)))
         (and db
              (let ((res (db-ref db str)))
                (close-db db)
                res))))
     (define (white-list? . o) (apply in-list? (white-list-file) o))
     (define (black-list? . o) (apply in-list? (black-list-file) o))
     (define (set-in-list! file . o)
       (let ((str (if (pair? o)
                      (car o)
                      (car (parse-mail-address (from)))))
             (db (open-db file)))
         (cond
          (db
           (db-set! db str #t)
           (close-db db)))))
     (define (white-list) (set-in-list! (white-list-file)))
     (define (black-list) (set-in-list! (black-list-file)))
     (define (auto-list-dest id mail)
       (string-append (string-downcase (car (string-split id ".@"))) "/"))
     (define (auto-list-name str)
       (let ((i (string-index str #\<))
             (j (string-index-right str #\>)))
         (if (and i j)
             (substring str (+ i 1) j)
             (let ((ls (string-split str)))
               (if (string-index (car ls) #\.)
                   (car ls)
                   (last ls))))))
     (define (auto-list . o)
       (let* ((rename (or (and (pair? o) (car o)) auto-list-dest))
              (addrs (cons (current-address) (if (pair? o) (cdr o) '())))
              (list-id
               (cond ((or (header "List-Id")
                          (header "X-Mailing-List"))
                      => auto-list-name)
                     (else #f))))
         (and list-id
              (let* ((dest (rename list-id (current-mail)))
                     (dest-path
                      (expand-user-dir (current-user-name)
                                       (string-append "~/Mail/" dest))))
                ;; if the destination folder already exists, or if the
                ;; mail wasn't sent to the user directly, we file it
                ;; in the list's folder
                (and (or (not (apply to/cc? addrs))
                         (file-exists? dest-path))
                     dest)))))
     ))

(define hato-sys-env-definitions
  (remove (lambda (x)
            (and (pair? x)
                 (eq? 'define (car x))
                 (pair? (cdr x))
                 (memq (if (pair? (cadr x)) (caadr x) (cadr x))
                       '(open-db default-folder spam-db-file
                         message-id-file is-duplicate? domain-key-verify
                         white-list white-list? white-list-file
                         black-list black-list? black-list-file
                         auto-list-dest auto-list
                         current-address current-mail))))
          hato-user-env-definitions))

(define (hato-user-env user . o)
  (let ((e (make-user-environment user))
        (expand-user-dir (if (pair? o) (car o) identity)))
    (safe-environment-extend!
     e
     feature-fold expand-user-dir file-exists?
     mstats? make-mstats mstats-urls mstats-ips mstats-emails mstats-urls
     mail? mail-text set-mail-text!
     mail-headers set-mail-headers! mail-stats set-mail-stats!
     mstats-prob mstats-urls mstats-emails mstats-ips
     (%open-db open-db) close-db db-ref db-set!
     string->keyword keyword->string
     mime-ref mime-parse-content-type mime-headers->list
     parse-mail-address parse-mail-address-list
     (%domain-key-verify domain-key-verify)
     ;; some handy srfi-1 funcs
     append-map any every filter remove fold find member last
     ;; some srfi-13 funcs
     string-index string-index-right string-ci=?
     string-contains string-contains-ci
     string-upcase string-downcase
     ;; regexp utils
     string-match string-search string-split
     ;; smtp address utils
     domain-part local-part
     )
    (safe-environment-set! e 'current-address (make-parameter #f))
    (safe-environment-set! e 'current-mail (make-parameter #f))
    (safe-eval hato-user-env-definitions environment: e)
    ;; don't give direct access to feature-fold or %open-db
    (safe-environment-remove! e 'feature-fold)
    (safe-environment-remove! e '%open-db)
    e))

)
