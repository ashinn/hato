
(require-library posix srfi-1 srfi-13 safe-io domain-keys hato-db hato-utils
                 hato-mime hato-archive hato-smtp hato-token hato-prob)

(module hato-filter-env
    (open-db default-folder spam-db-file headers header
     to cc to/cc from to? cc? to/cc? from? match-addrs
     date subject mailer message-id size domain-key-verify
     get-stats spam-probability is-spam? urls emails ips
     message-id-file is-duplicate? in-list? set-in-list!
     white-list-file black-list-file white-list? black-list?
     white-list black-list
     auto-list-dest auto-list-name auto-list
     make-mail mail? mail-text set-mail-text!
     mail-headers set-mail-headers! mail-stats set-mail-stats!
     current-address current-mail
     )

(import (rename scheme
                (open-input-file %open-input-file)
                (open-output-file %open-output-file))
        (rename hato-db (open-db %open-db))
        (rename domain-keys (domain-key-verify %domain-key-verify))
        chicken extras posix ports data-structures srfi-1 srfi-13 safe-io
        hato-utils hato-mime hato-archive hato-smtp hato-token hato-prob)

(define-record-type <mail>
  (%make-mail text headers stats)
  mail?
  (text mail-text set-mail-text!)
  (headers mail-headers set-mail-headers!)
  (stats mail-stats set-mail-stats!))

(define (make-mail . o)
  (let-optionals* o ((text "") (headers #f) (stats #f))
    (%make-mail text headers stats)))

(define current-address (make-parameter #f))
(define current-mail (make-parameter #f))

(define open-db
  (let ((%open-db %open-db))
    (lambda (file . o)
      (let* ((read-only? (and (pair? o) (car o)))
             (check (if read-only? file-readable? file-writeable?))
             (ok? (check file)))
        (if (eq? ok? 'new)
            (touch-file file (current-safe-user-id) (current-safe-group-id)))
        (if ok?
            (%open-db file read-only?)
            (error (if read-only?
                       "file not readable: "
                       "file not writeable: ")
                   file))))))

;; (define (default-folder)
;;   (hato-default-user-folder (current-safe-user-id)))

(define (default-folder)
  (expand-user-path (current-safe-user-id) "~/Mail/"))

(define (spam-db-file)
  (expand-user-path (current-safe-user-id) "~/.hato/spam.db"))

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

(define (to/cc)
  (append (to) (cc)))

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

(define (urls)   (mstats-urls (get-stats)))

(define (emails) (mstats-emails (get-stats)))

(define (ips)    (mstats-ips (get-stats)))

(define (message-id-file)
  (expand-user-path (current-safe-user-id) "~/.hato/msgid.db"))

(define (is-duplicate?)
  (let* ((id (message-id))
         (file (message-id-file))
         (db (open-db file)))
    (and db
         (let ((res (db-ref db id)))
           (if (not res)
               (db-set! db id (current-seconds)))
           (close-db db)
           res))))

(define (white-list-file)
  (expand-user-path (current-safe-user-id) "~/.hato/white.db"))

(define (black-list-file)
  (expand-user-path (current-safe-user-id) "~/.hato/black.db"))

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
         (not (string->number list-id))
         (let* ((dest (rename list-id (current-mail)))
                (dest-path
                 (expand-user-path (current-safe-user-id)
                                   (string-append "~/Mail/" dest))))
           ;; if the destination folder already exists, or if the
           ;; mail wasn't sent to the user directly, we file it
           ;; in the list's folder
           (and (or (not (apply to/cc? addrs))
                    (file-exists? dest-path))
                dest)))))

)

