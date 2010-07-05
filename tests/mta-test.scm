#!/usr/local/bin/csi -script

;; So how do you unit test a mail server?
;; We need to specify at least:
;;
;;  1) config path
;;  2) base dir
;;  3) listen port
;;  4) virtual (non-OS) users
;;
;;    * the config can specify 2 & 3
;;    * if running as non-root and the default port, 5025, is open we
;;      don't need to specify 3
;;
;; What this framework can't test:
;;
;;  1) sanity checks running as root (correct dirs are setup, etc.)
;;  2) delivery checks as root (correct file owners and permissions)
;;  3) tests on non-local connections (SPF, blacklists, etc.)
;;
;; 1 & 2 are effected by a relatively small amount of code which likely
;; won't change much.  To verify these it should generally be enough to
;; run a few manual tests on a staging server after confirming the
;; automatic tests.
;;
;; 3 could conceivably be tested using an RPC framework, though I'm
;; making this a low priority for now.
;;
;; WARNING: DO NOT USE ANY REAL EMAIL ADDRESSES IN TESTS!
;;
;; The simplest way to do this is to use non-valid top-level domains
;; (e.g. foo.bar, evil.company, etc.).

(use srfi-1 regex extras posix test hato-smtp hato-mime hato-archive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; first things first - never run this test suite as root!

(if (zero? (current-user-id))
    (error "won't run the tests as root"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test-port 5025)

(define (send-test-mail file)
  (let* ((headers (with-input-from-file file mime-headers->list))
         (from (car (parse-mail-address (mime-ref headers "from"))))
         (to (map car (parse-mail-address-list (mime-ref headers "to"))))
         (res
          (call-with-input-file file
            (cut send-mail Port: test-port From: from To: to Source: <>
                 No-Sendmail: #t)))
         (expect-fail? (string-search "fail" file)))
    (test-assert (sprintf "send-mail ~A~A ~A"
                          file
                          (if expect-fail? " [expect failure]" "")
                          res)
                 (if expect-fail? (pair? res) (null? res)))))

(define (mime-equal? a b)
  (and (equal? (mime-ref (car a) "subject") (mime-ref (car b) "subject"))
       (equal? (cdr a) (cdr b))))

(define (mail-equal? a b)
;;   (and (= (length a) (length b))
;;        (every mime-equal? a b))
  (call-with-input-string a
    (lambda (a-in)
      (call-with-input-string b
        (lambda (b-in)
          (let* ((a-headers (mime-headers->list a-in))
                 (a-body (read-all a-in))
                 (b-headers (mime-headers->list b-in))
                 (b-body (read-all b-in)))
            (and (equal? (mime-ref a-headers "subject")
                         (mime-ref b-headers "subject"))
                 (equal? a-body b-body))))))))

(define (mail-archive-equal? a b)
  (let* ((msg-a (mail-archive->list a))
         (msg-b (mail-archive->list b)))
    (test-assert (sprintf "~A = ~A"
                          (substring a (+ 1 (or (substring-index "/" a) -1)))
                          (substring b (+ 1 (or (substring-index "/" b) -1))))
                 (and (= (length msg-a) (length msg-b))
                      (every mail-equal? msg-a msg-b)))))

(define (file-equal? a b)
  (call-with-input-file a
    (lambda (a-in)
      (call-with-input-file b
        (lambda (b-in)
          (let lp ()
            (let ((a-line (read-line a-in))
                  (b-line (read-line b-in)))
              (cond
               ((eof-object? a-line) (eof-object? b-line))
               ((eof-object? b-line) #f)
               (else (and (equal? a-line b-line) (lp)))))))))))

(define (ls-a dir)
  (map (cut string-append dir "/" <>)
       (sort (directory dir #t) string<?)))

(define (copy-directory-tree a b)
  (cond
   ((directory? a)
    (if (file-exists? b)
        (if (not (directory? b))
            (error "can't create directory over existing file" a b))
        (create-directory b))
    (for-each
     (lambda (f)
       (copy-directory-tree (string-append a "/" f) (string-append b "/" f)))
     (directory a #t)))
   ((directory? b)
    (if (file-exists? a)
        (error "can't copy file over existing directory" a b)))
   (else
    (call-with-output-file b
      (lambda (out)
        (call-with-input-file a
          (lambda (in)
            (let lp ()
              (let ((str (read-string 1024 in)))
                (cond
                 ((not (or (eof-object? str) (equal? "" str)))
                  (write-string str #f out)
                  (lp))))))))))))

(define (mail-directories-compare a b)
  (cond
   ((and (eq? 'maildir (mail-archive-format a))
         (eq? 'maildir (mail-archive-format b)))
    ;; in the event of maildirs the file names won't match so we just
    ;; test that they're equivalent mail directories
    (mail-archive-equal? a b))
   (else
    (let lp ((a-files (remove (cut string-match ".*(~|\\.log)$" <>) (ls-a a)))
             (b-files (remove (cut string-match ".*(~|\\.log)$" <>) (ls-a b))))
      ;;(fprintf (current-error-port) "lp a: ~S b: ~S\n" a-files b-files)
      (cond
       ((null? a-files)
        (if (not (null? b-files))
            (test-assert (sprintf "extra files: ~S" b-files) #f)))
       ((null? b-files)
        (test-assert (sprintf "missing files: ~S" a-files) #f))
       (else
        (let ((a1 (car a-files))
              (b1 (car b-files)))
          (cond
           ((directory? a1)
            (and (test-assert (sprintf "(directory? ~S)" b1)
                              (directory? b1))
                 (mail-directories-compare a1 b1)))
           ((directory? b1) (test-assert (sprintf "not directory?: ~S" a1) #f))
           ((and (mail-archive-format a1) (mail-archive-format b1))
            (mail-archive-equal? a1 b1))
           (else
            (test-assert
             (sprintf "~A = ~A"
                      (substring a1 (+ 1 (or (substring-index "/" a1) -1)))
                      (substring b1 (+ 1 (or (substring-index "/" b1) -1))))
             (file-equal? a1 b1))))
          (lp (cdr a-files) (cdr b-files)))))))))

(define (with-test-server-from-dir dir thunk)

  ;; clean out any previous test files (keep the log files around)
  (for-each
   (lambda (d) (if (directory? d) (for-each delete-file (ls-a d))))
   (map (cut string-append dir <>)
        '("/root/var/queue" "/root/var/run" "/root/var/mail")))
  ;; ... wipe out the homes completely (be careful with this!)
  (letrec ((rm-rf
            (lambda (f)
              (if (directory? f)
                  (for-each
                   rm-rf
                   (map (lambda (x) (string-append f "/" x))
                        (directory f #t)))
                  (delete-file f)))))
    (rm-rf (string-append dir "/root/home")))
  ;; ... and re-populate the homes
  (copy-directory-tree (string-append dir "/init-home")
                       (string-append dir "/root/home"))

  ;; start the server
  (test-assert
   (sprintf "starting server in ~A" dir)
   (zero?
    (system
     (sprintf
      "cd .. && ./hato-mta.scm -d --virtual --port ~A --base tests/~A/root/"
      test-port dir))))

  ;; do whatever
  (thunk)

  ;; kill the server
  (test-assert
   (sprintf "stopping server in ~A" dir)
   (zero?
    (system
     (sprintf "cd .. && ./hato-mta.scm -d --base tests/~A/root/ --kill"
              dir)))))

(define (test-send-mail-dir dir)

  ;;(fprintf (current-error-port) "testing ~A\n" dir)

  (if (not (and (directory? (string-append dir "/root"))
                (directory? (string-append dir "/send-queue"))
                (directory? (string-append dir "/verify-mail"))))
      (fprintf (current-error-port)
               "WARNING: skipping mis-configured test directory ~A\n" dir)
      (with-test-server-from-dir dir
        (lambda ()

          ;; send the mails
          (for-each
           send-test-mail
           (filter (cut string-match "^.*test-.*[^~]$" <>)
                   (ls-a (string-append dir "/send-queue"))))

          ;; give a little time for them to be delivered
          (sleep 1)

          ;; verify mails, modulo delivery dates and server name, etc.
          (mail-directories-compare
           (string-append dir "/verify-mail")
           (string-append dir "/root/var/mail"))

          (mail-directories-compare
           (string-append dir "/verify-home")
           (string-append dir "/root/home"))

          ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "hato")

(for-each test-send-mail-dir (filter directory? (ls-a "roots")))

(test-end "hato")

