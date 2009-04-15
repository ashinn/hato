;;;; hato-archive.scm -- mbox, MH and Maildir utilities
;;
;; Copyright (c) 2005-2008 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; Procedure: mail-archive-add archive [message] [format] [headers]
;;                                     [mode] [user] [group] [nl]
;;
;;   Adds the text MIME message MESSAGE (a string or port, default
;;   current-input-port) to the mail archive specified by the string
;;   ARCHIVE.  FORMAT should be one of the lowercase symbols 'mbox,
;;   'maildir or 'mh.  If absent, then ARCHIVE is examined to determine
;;   what type of format to use:
;;
;;     regular file - 'mbox
;;     non-existent - 'mbox
;;     directory with "cur" "new" and "tmp" subdirs - 'maildir
;;     other directories - 'mh
;;
;;   HEADERS may be passed for use in envelope information added to the
;;   mbox format.
;;
;;   MODE, USER and GROUP optionally specify the mode and owner
;;   information for new files.  USER and GROUP may be integers or
;;   strings, or alternately #f or -1 to specify no change.  You must be
;;   root to specify a user or group other than the current process'
;;   user and group.
;;
;;   NL is the newline convention, used for the initial From line, and
;;   defaults to the output of (newline).
;;
;;      Rationale: this may seem like overkill but I want to use the
;;      same library for both servers and simple mail clients, and this
;;      interface minimizes the number of system calls that need to be
;;      made.
;;
;;   Returns the name of the file written.
;;
;; Procedure: mail-archive-format archive
;;
;;   Examines the file ARCHIVE to determine it's format, or #f if the
;;   file does not exist.
;;
;; Procedure: create-maildir path
;;
;;   Creates a maildir-format archive for the give path, returning #f on
;;   failure and a true value otherwise.

;; See: http://cr.yp.to/proto/maildir.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional mail utils (may be moved elsewhere):
;;
;; Procedure: parse-mail-address string
;;
;;   Parses a mail address in any common format and returns a list of
;;   two elements, the actual email address (trimmed) and the persons'
;;   name (or "" if no name given).
;;
;; Procedure: parse-mail-address-list string
;;
;;   Parses a comma-delimited list of email addresses, each parsed
;;   according to parse-mail-address.

(cond-expand
 ((and compiling (not static))
  (declare
   (export mail-archive-add mail-archive-remove mail-archive-read
           mail-archive-fold mail-archive-for-each mail-archive->list
           mail-archive-format
           mh-next-name maildir-next-name generate-maildir-name
           parse-mail-address parse-mail-address-list
           create-maildir
           current-mbox-date-string
           )))
 (else))

(use posix hato-mime extras utils regex srfi-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (current-seconds-string)
  (let* ((now (number->string (current-seconds)))
         (now-len (string-length now)))
    (if (eqv? #\. (string-ref now (- now-len 2)))
      (substring now 0 (- now-len 2))
      now)))

;; returns (address name)
(define parse-mail-address
  (let* ((name-pat "([^\\\"\\s][^<\\\"]*[^\\\"\\s])")
         (addr-pat "([^\\\"\\s@]+@[^\\\"\\s@]+)")
         (rx-name-<addr>
          (regexp (string-append "\\s*" name-pat "\\s*<" addr-pat ">\\s*")))
         (rx-qname-<addr>
          (regexp (string-append "\\s*\"([^\\\"]*)\"\\s*<" addr-pat ">\\s*")))
         (rx-addr-pname
          (regexp (string-append "\\s*" addr-pat "\\s*\\("name-pat"\\)\\s*")))
         (rx-<addr> (regexp (string-append "\\s*<" addr-pat ">\\s*")))
         (rx-trim (regexp "\\s*([^\\s](?:.*[^\\s])?)\\s*"))
         (revmatch (lambda (m) (list (caddr m) (cadr m)))))
    (lambda (addr)
      (and (string? addr)
           (cond
            ((string-match rx-name-<addr> addr) => revmatch)
            ((string-match rx-qname-<addr> addr) => revmatch)
            ((string-match rx-addr-pname addr) => cdr)
            ((string-match rx-<addr> addr)
             => (lambda (m) (list (cadr m) "")))
            ((string-match rx-trim addr)
             => (lambda (m) (list (cadr m) "")))
            (else (list addr "")))))))

(define (parse-mail-address-list str)
  (map parse-mail-address (string-split str ",")))

(define-constant error-limit 20)

(define-inline (mode->file-type i) ; XXXX *nix specific
  (case (arithmetic-shift i -12)
    ((#o01) 'pipe)  ((#o02) 'char) ((#o04) 'directory)
    ((#o06) 'block) ((#o12) 'link) ((#o14) 'link)
    (else   'file)))

(define delivery-count
  (let ((i 0))
    (lambda () (set! i (+ i 1)) i)))

(define (mail-archive-format file)
  (condition-case
      (let ((vec (file-stat file)))
        (case (mode->file-type (vector-ref vec 1))
          ((file)
           (if (or (zero? (vector-ref vec 5))
                   (string-match
                    "^(From |[-_+a-zA-Z0-9]+:).*"
                    (call-with-input-file file read-line)))
               'mbox
               #f))
          ((link)
           (let ((dest (read-symbolic-link file)))
             (mail-archive-format
              (if (eqv? #\/ (string-ref dest 0))
                  dest
                  (string-append (pathname-directory file) dest)))))
          ((directory)
           (if (and (>= (vector-ref vec 2) 3) ; shortcut check for link-count
                    (or (directory? (string-append file "/cur"))
                        (directory? (string-append file "/new")))
                    ;;(directory? (string-append file "/tmp"))
                    )
               'maildir
               'mh))
          (else
           #f)))
    (exn () #f)))

(define (mail-new-archive-format file)
  (let ((len (string-length file)))
    (cond
     ((and (> len 0) (eqv? #\/ (string-ref file (- len 1))))
      (if (not (file-exists? file))
          (create-directory file))
      'mh)
     ((and (> len 1)
           (eqv? #\. (string-ref file (- len 1)))
           (eqv? #\/ (string-ref file (- len 2))))
      (if (not (file-exists? file))
          (begin
            (create-directory file)
            (create-directory (string-append file "/cur"))
            (create-directory (string-append file "/new"))
            (create-directory (string-append file "/tmp"))))
      'maildir)
     (else
      (if (directory? file) 'mh 'mbox)))))

(define (generate-maildir-name)
  (sprintf "~A.P~AQ~A.~A"
           (current-seconds-string)
           (current-process-id)
           (delivery-count)
           (get-host-name)))

(define (maildir-next-name dir . o)
  (let-optionals* o ((mode #o600) (uid #f) (gid #f))
    (let lp ((i 0))
      (let ((path (string-append dir "/cur/" (generate-maildir-name))))
        (condition-case
            (let* ((fd (file-open path
                                  (bitwise-ior open/rdwr open/creat open/excl)
                                  mode))
                   (port (open-output-file* fd)))
              (ensure-file-owner path uid gid)
              (values port path))
          (exn ()
               (cond
                ((< i error-limit)
                 (for-each
                  (lambda (d)
                    (cond
                     ((not (directory? d))
                      (create-directory d)
                      (ensure-file-owner d uid gid)
                      )))
                  (cons dir
                        (map (lambda (d) (string-append dir d))
                             '("/cur" "/new" "/tmp"))))
                 (lp (+ i 1)))
                (else
                 (values #f #f)))))))))

(define (mh-highest-name dir)
  (condition-case
      (let lp ((ls (directory dir)) (hi 0))
        (if (null? ls)
            hi
            (lp (cdr ls) (max hi (or (string->number (car ls)) 0)))))
    (exn () 0)))

(define (mh-next-name dir mode uid gid)
  (let ((start (+ 1 (mh-highest-name dir))))
    (cond
     ((and (= start 1) (not (file-exists? dir)))
      (create-directory dir)
      (ensure-file-owner dir uid gid)))
    (let lp ((i start) (j 0))
      (condition-case
          (let* ((path (sprintf "~A/~A" dir i))
                 (fd (file-open path
                                (bitwise-ior open/rdwr open/creat open/excl)
                                mode))
                 (port (open-output-file* fd)))
            (ensure-file-owner path uid gid)
            (values port path))
        (exn ()
             (cond
              ((not (directory? dir))
               (create-directory dir)
               (ensure-file-owner dir uid gid)))
             (if (< j error-limit)
                 (lp (+ i 1) (+ j 1))
                 (values #f #f)))))))

;; The date inserted at the start of a message in archive files.
;; Historically it seems sendmail chose the format
;;
;;   From address Tue Nov  8 04:18:09 2005
;;
;; possibly before there was a standardized date format in RFC 822.
;; Other mail servers seemed to follow suit, using the standard RFC
;; 822 format for mail headers, but sticking with the old format for
;; mbox.  Some popular mail tools (mutt, at least) specifically
;; require this exact format and will ignore any file with any other
;; first line, including those using RFC 822 format (which is in fact
;; the opposite of mutt's documentation, but that's a different
;; story).  So for the time being we're stuck with this nonsense.
(define (current-mbox-date-string)
  (let ((s (seconds->string (current-seconds))))
    (substring s 0 (- (string-length s) 1))))

(define (get-user-id user)
  (if (number? user)
    user
    (and-let* (((string? user))
               (ls (user-information user)))
      (caddr ls))))

(define (get-group-id group)
  (if (number? group)
    group
    (and (string? group)
         (receive (name . o) (group-information group)
           (and name (cadr o))))))

(define (ensure-file-owner file uid gid)
  (if (and (or uid gid) (not (and (eqv? uid -1) (eqv? gid -1))))
      (let ((uid (or (get-user-id uid) -1))
            (gid (or (get-group-id gid) -1)))
        (change-file-owner file uid gid))))

(define (mail-archive-add file . o)
  (let-optionals* o ((src (current-input-port))
                     (format #f)
                     (headers #f)
                     (mode #o600)
                     (uid #f)
                     (gid #f)
		     (nl (call-with-output-string newline)))
    (case (or format
              (if (file-exists? file)
                  (mail-archive-format file)
                  (mail-new-archive-format file)))
      ((mbox)
       (let* ((fd (file-open file
                             (bitwise-ior open/append open/write open/creat)
                             mode))
              (port (open-output-file* fd))
              (lock (file-lock port)))
         (set-file-position! port 0 seek/end)
         (if (zero? (file-position port))
             (ensure-file-owner file uid gid)
             (display nl port))
         ;; output the mbox From line
         (if (port? src)
             (let ((line (read-line src)))
               (cond
                ((not (substring=? line "From " 0 0 5))
                 (display "From " port) ; XXXX fetch sender+date
                 (display nl port)))
               (display line port)
               (display nl port))
             (if (not (substring=? src "From " 0 0 5))
                 (let* ((h (or headers
                               (with-input-from-string src
                                 mime-headers->list)))
                        (from (or (mime-ref h "sender") (mime-ref h "from" "")))
                        (from-addr (and (string? from)
                                        (parse-mail-address from)))
                        (date (current-mbox-date-string)))
                   (fprintf port "From ~A ~A~A"
                            (if (pair? from-addr) (car from-addr) "<unknown>")
                            date
                            nl))))
         ;; output the body of the message, and ensure a trailing
         ;; newline
         (cond
          ((port? src)
           (let lp ((last "x"))
             (let* ((chunk (read-string 1024 src))
                    (len (string-length chunk)))
               (display chunk port)
               (if (< len 1024)
                   (if (not (eqv? #\newline
                                  (string-ref last (- (string-length last) 1))))
                       (display nl port))
                   (lp chunk)))))
          (else
           (display src port)
           (if (not (eqv? #\newline
                          (string-ref src (- (string-length src) 1))))
               (display nl port))))
         (file-unlock lock)
         (close-output-port port)
         file))
      ((maildir mh)
       (receive (port name)
           ((if (eq? format 'mh) mh-next-name maildir-next-name)
            file mode uid gid)
         (display (if (port? src) (read-all src) src) port)
         (close-output-port port)
         name))
      (else (error "unknown mail archive format" format)))))

(define (create-maildir path)
  (condition-case
      (for-each
       (lambda (d)
         (if (not (directory? d))
             (create-directory path)))
       (list path (map (cut string-append path "/" <>)
                       '("cur" "new" "tmp"))))
    (exn () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (delete-files-in-directory dir ls)
  (every
   (lambda (f)
     (condition-case (begin (delete-file (string-append dir "/" f)) #t)
       (exn () #f)))
   ls))

(define (mail-archive-remove archive messages)
  (let ((format (mail-archive-format archive))
        (messages (if (not (or (pair? messages) (null? messages)))
                      (list messages)
                      messages)))
    (case format
      ((maildir)
       (let ((dir (string-append archive "/cur/")))
         (receive (nums strings) (partition number? messages)
           (let ((files (if (null? nums)
                            strings
                            (let ((files (sort (directory dir) string<?)))
                              (append
                               (map (lambda (n) (list-ref files (- n 1))) nums)
                               strings)))))
             (delete-files-in-directory dir files)))))
      ((mh)
       (or (null? messages)
           (let ((messages
                  (sort
                   (map (lambda (x)
                          (if (string? x)
                              (or (string->number x)
                                  (error "invalid mh message" x))
                              x))
                        messages)
                   <))
                 (move-message
                  (lambda (from to)
                    (let ((from
                           (string-append archive "/" (number->string from)))
                          (to
                           (string-append archive "/" (number->string to))))
                      (and (not (file-exists? to))
                           (rename-file from to)))))
                 (remove-message
                  (lambda (n)
                    (delete-files-in-directory
                     archive
                     (list (number->string n))))))
             ;; delete and renumber messages with minimal renaming,
             ;; stop and return #f on any failure
             (and (remove-message (car messages))
                  (let lp ((i (car messages)) (ls (cdr messages)))
                    (and (pair? ls)
                         (let rem ((j (+ i 1)) (ls ls))
                           (cond
                            ((and (pair? ls)
                                  (= j (car ls)))
                             (and (remove-message (car ls))
                                  (rem (+ j 1) (cdr ls))))
                            (else
                             (let ((limit
                                    (if (pair? ls)
                                        (car ls)
                                        (+ 1 (mh-highest-name archive)))))
                               (let mov ((from j) (to i))
                                 (if (>= from limit)
                                     (or (null? ls)
                                         (and (remove-message (car ls))
                                              (lp (car ls) (cdr ls))))
                                     (and (move-message from to)
                                          (mov (+ 1 from) (+ 1 to)))
                                   ))))))))))))
      ((mbox)
       ;; XXXX this would be easier and faster with my text-buffer
       ;; implementation
       (let lp ((ls messages) (offset 0))
         (or (null? ls)
             (and (mbox-remove-one archive (+ (car ls) offset))
                  (lp (cdr ls) (+ 1 offset))))))
      (else
       (or (not (file-exists? archive)) ; an "empty" archive
           (error "unknown format" format))))))

(define (mbox-remove-one file n)
  (receive (from to)
      (call-with-input-file file
        (lambda (in)
          (let lp ((i 1) (from-len 0))
            (let ((pos (file-position in)))
              (receive (lines next-from) (mail-archive-read-lines in)
                (let ((next-len (if (eof-object? next-from)
                                    0
                                    (+ 1 (string-length next-from)))))
                  (if (>= i n)
                      (values (- pos from-len)
                              (- (file-position in) next-len))
                      (lp (+ i 1) next-len))))))))
    (file-excise-byte-range file from to)))

(define (file-excise-byte-range file from to)
  (or (>= from to)
      (let ((fd (file-open file open/rdwr))
            (end (file-size file))
            (diff (+ 1 (- to from)))
            (buf-size 1024))
        (let lp ((from from) (to to))
          (set-file-position! fd to)
          (let ((chunk (file-read fd buf-size)))
            (set-file-position! fd from)
            (cond
             ((>= (+ from buf-size) end)
              (file-write fd (car chunk) (cadr chunk)))
             (else
              (file-write fd (car chunk))
              (lp (+ from buf-size) (+ to buf-size))))))
        (file-truncate fd (- end diff))
        (file-close fd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mail-archive-read-lines src . o)
  (let ((offset (and (pair? o) (car o)))
        (port (if (string? src) (open-input-file src) src)))
    (if offset
        (set-file-position! port offset seek/set))
    (let ((first-line (read-line port)))
      (if (eof-object? first-line)
          (values '() first-line)
          (let lp ((res (list first-line)))
            (let ((line (read-line port)))
              (cond
               ((or (eof-object? line)
                    (and (>= (string-length line) 5)
                         (string=? "From " (substring line 0 5))))
                (if (string? src)
                    (close-input-port port))
                (values (reverse res) line))
               (else
                (lp (cons line res))))))))))

(define (mail-archive-read src . o)
  (receive (lines next-from) (apply mail-archive-read-lines src o)
    (values (string-intersperse lines
                                (call-with-output-string newline))
            next-from)))

(define (mail-files dir)
  (map (lambda (f) (string-append dir "/" f))
       (remove (lambda (f)
                 (or (equal? "" f)
                     (not (char-numeric? (string-ref f 0)))
                     (eqv? #\~ (string-ref f (- (string-length f) 1)))))
               (directory dir))))

(define (mail-archive-fold src kons knil)
  (cond
   ((and (string? src) (directory? src))
    (let* ((cur (string-append src "/cur"))
           (cur? (directory? cur))
           (new (string-append src "/new"))
           (new? (directory? new))
           (maildir? (or cur? new?)))
      (fold (lambda (f acc)
              (receive (text next-from)
                  (call-with-input-file f mail-archive-read)
                (kons (pathname-strip-directory f) text acc)))
            knil
            (sort
             (if maildir?
                 (append
                  (if new? (mail-files new) '())
                  (if cur? (mail-files cur) '()))
                 (mail-files src))
             (if maildir?
                 string<?
                 (lambda (a b)
                   (let ((a^ (string->number a))
                         (b^ (string->number b)))
                     (if (and a^ b^)
                         (< a^ b^)
                         (string<? a b)))))))))
   (else
    (let ((port (if (string? src) (open-input-file src) src)))
      (let lp ((i 1) (res knil) (from-line #f))
        (receive (lines next-from) (mail-archive-read-lines port)
          (let ((res (if (pair? lines)
                         (kons i
                               (string-intersperse
                                (if (string? from-line)
                                    (cons from-line lines)
                                    lines)
                                (call-with-output-string newline))
                               res)
                         lines)))
            (cond
             ((eof-object? next-from)
              (if (string? src)
                  (close-input-port port))
              res)
             (else
              (lp (+ i 1) res next-from))))))))))

(define (mail-archive-for-each src proc)
  (mail-archive-fold src (lambda (f text res) (proc text)) #f))

(define (mail-archive->list src)
  (reverse
   (mail-archive-fold src (lambda (f text res) (cons text res)) '())))

