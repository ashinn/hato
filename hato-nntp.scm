;;;; hato-nntp.scm -- nntp client library
;;
;; Copyright (c) 2008-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use autoload tcp extras utils posix srfi-1)
(autoload hato-smtp smtp-write-message)
(autoload hato-mime mime-headers->list)

(define-record-type nntp
  (make-nntp in out status message debug? locked?)
  nntp?
  (in nntp-in)
  (out nntp-out)
  (status nntp-status nntp-status-set!)
  (message nntp-message nntp-message-set!)
  (debug? nntp-debug?)
  (locked? nntp-locked? nntp-locked?-set!)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond-expand
 ((and chicken compiling)
  (declare
   (export nntp-connect nntp-disconnect nntp-status nntp-message
           nntp-article nntp-body nntp-group nntp-head
           nntp-ihave nntp-last nntp-list nntp-newgroups
           nntp-newnews nntp-next nntp-post nntp-quit
           nntp-slave nntp-stat nntp-list->list
           nntp-head->alist call-with-nntp-article
           nntp-newgroups-fold nntp-newnews-fold nntp?
           post-article
           )))
 (else))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (nntp-connect host . o)
  (let-optionals* o ((debug? #f)
                     (port 119))
    (receive (in out) (tcp-connect host port)
      (and in out
           (let ((nntp (make-nntp in out #f #f debug? #f)))
             (nntp-get-response nntp)
             nntp)))))

(define (port-open? port)
  (and (port? port)
       (not (##sys#slot port 8))))

(define (nntp-disconnect nntp)
  (let ((res
         (or (condition-case (nntp-quit nntp) (exn () #f))
             (begin
               (if (port-open? (nntp-out nntp))
                   (display "QUIT\r\n" (nntp-out nntp)))
               #f))))
    (close-output-port (nntp-out nntp))
    (close-input-port (nntp-in nntp))
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (nntp-send nntp cmd . args)
  (if (nntp-debug? nntp)
      (fprintf (current-error-port) "nntp-send: ~S ~S\n" cmd
               (if (memq cmd '(PASS Pass pass)) '("********") args)))
  (cond
   ((nntp-locked? nntp)
    (fprintf
     (current-error-port)
     "nntp-send: warning, can't send '~S ~S' while locked by command ~S\n"
     cmd args (nntp-locked? nntp))
    #f)
   (else
    (let ((out (nntp-out nntp)))
      (display cmd out)
      (for-each (lambda (x) (write-char #\space out) (display x out)) args)
      (newline out)
      (nntp-get-response nntp)))))

(define (nntp-get-response nntp)
  (let lp ((line (read-line (nntp-in nntp))) (msg '()))
    (cond
     ((eof-object? line)
      (nntp-disconnect nntp)
      #f)
     (else
      (if (nntp-debug? nntp)
          (fprintf (current-error-port) "nntp-resp: ~A\n" line))
      (let ((len (string-length line)))
        (if (and (>= len 4) (eqv? #\- (string-ref line 3)))
            (lp (read-line (nntp-in nntp)) (cons (substring line 4) msg))
            (and-let* (((>= len 3))
                       (status (string->number (substring line 0 3)))
                       (str (if (= len 3) "" (substring line 4))))
              (nntp-status-set! nntp status)
              (nntp-message-set! nntp (string-intersperse (cons str msg) " "))
              (< status 400))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (nntp-quit nntp) (nntp-send nntp 'QUIT))
(define (nntp-slave nntp) (nntp-send nntp 'SLAVE))
(define (nntp-stat nntp) (nntp-send nntp 'STAT))
(define (nntp-post nntp) (nntp-send nntp 'POST))
(define (nntp-last nntp) (nntp-send nntp 'LAST))
(define (nntp-next nntp) (nntp-send nntp 'NEXT))
(define (nntp-ihave nntp msgid) (nntp-send nntp 'IHAVE msgid))
(define (nntp-group nntp group) (nntp-send nntp 'GROUP group))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-nntp-data-port cmd nntp)
  (let ((in (nntp-in nntp))
        (buf "")
        (i 2)
        (len 0))
    (define (fill-buffer!)
      (let ((line (read-line in)))
        (cond
         ((or (eof-object? line) (equal? "." line))
          (set! buf #f)
          (set! i 2)
          (set! len 0))
         ((and (not (equal? "" line)) (eqv? #\. (string-ref line 0)))
          (set! buf (substring line 1))
          (set! i 0)
          (set! len (string-length buf)))
         (else
          (set! buf line)
          (set! i 0)
          (set! len (string-length line))))))
    (define (read-char)
      (cond ((< i len) (set! i (+ i 1)) (string-ref buf (- i 1)))
            ((= i len) (set! i (+ i 1)) #\return)
            ((= i (+ len 1)) (set! i (+ i 1)) #\newline)
            (else
             (fill-buffer!)
             (if (string? buf)
                 (read-char)
                 #!eof))))
    (define (peek-char)
      (cond ((< i len) (string-ref buf i))
            ((= i len) #\return)
            ((= i (+ len 1)) #\newline)
            (else
             (fill-buffer!)
             (if (string? buf)
                 (read-char)
                 #!eof))))
    (define (char-ready?)
      (or (<= i (+ len 1)) (and (fill-buffer!) (string? buf))))
    (define (close)
      (if buf
          (let lp ()
            (let ((line (read-line in)))
              (if (not (or (eof-object? line) (equal? "." line)))
                  (lp)))))
      (nntp-locked?-set! nntp #f))
    (nntp-locked?-set! nntp cmd)
    (make-input-port read-char char-ready? close peek-char)))

(define (nntp-article nntp article)
  (and (nntp-send nntp 'ARTICLE article)
       (make-nntp-data-port 'ARTICLE nntp)))

(define (nntp-head nntp article)
  (and (nntp-send nntp 'HEAD article)
       (make-nntp-data-port 'HEAD nntp)))

(define (nntp-body nntp article)
  (and (nntp-send nntp 'BODY article)
       (make-nntp-data-port 'BODY nntp)))

(define (call-with-nntp-article nntp msg proc)
  (let ((port (nntp-article nntp msg)))
    (and port
         (let ((res (condition-case (proc port)
                      (exn ()
                        (close-input-port port)
                        (signal exn)))))
           (close-input-port port)
           res))))

(define (nntp-head->alist nntp msg)
  (let ((port (nntp-article nntp msg)))
    (and port
         (let ((res (condition-case (mime-headers->list port)
                      (exn ()
                        (close-input-port port)
                        (signal exn)))))
           (close-input-port port)
           res))))

(define (nntp-list nntp)
  (and (nntp-send nntp 'LIST)
       (make-nntp-data-port 'LIST nntp)))

(define (nntp-port-fold nntp port kons knil)
  (and port
       (let ((res (condition-case
                      (let lp ((res knil))
                        (let ((line (read-line port)))
                          (if (or (eof-object? line)
                                  (equal? "." line))
                              res
                              (lp (kons line res)))))
                    (exn ()
                         (close-input-port port)
                         (signal exn)))))
         (close-input-port port)
         res)))

(define (nntp-list-fold nntp kons knil)
  (nntp-port-fold
   (nntp-list nntp)
   (lambda (line res)
     (kons (map (lambda (x) (or (string->number x) x)) (string-split line)) res))
   knil))

(define (nntp-list->list nntp)
  (nntp-list-fold nntp cons '()))

(define (number->string/pad2 n)
  (if (< n 10)
      (string-append "0" (number->string n))
      (number->string n)))

(define (nntp-date->string vec)
  (string-append (number->string/pad2 (vector-ref vec 5))
                 (number->string/pad2 (vector-ref vec 4))
                 (number->string/pad2 (vector-ref vec 3))))

(define (nntp-time->string vec)
  (string-append (number->string/pad2 (vector-ref vec 2))
                 (number->string/pad2 (vector-ref vec 1))
                 (number->string/pad2 (vector-ref vec 0))))

(define (nntp-dists->string dists)
  (string-append
   "<"
   (string-intersperse (map ->string dists) ",")
   ">"))

(define (nntp-newgroups nntp dt . o)
  (let-optionals* o ((gmt? #f)
                     (dists '()))
    (let* ((dt (if (number? dt) (seconds->local-time dt) dt))
           (cmd-ls `(,(nntp-date->string dt)
                     ,(nntp-time->string dt)
                     ,@(if gmt? '("GMT") '())
                     ,@(if (pair? dists)
                           (list (nntp-dists->string dists))
                           '()))))
      (and (apply nntp-send nntp 'NEWGROUPS cmd-ls)
           (make-nntp-data-port 'NEWGROUPS nntp)))))

(define (nntp-newgroups-fold nntp kons knil . args)
  (nntp-port-fold (apply nntp-newgroups nntp args) kons knil))

(define (nntp-newnews nntp groups dt . o)
  (let-optionals* o ((gmt? #f)
                     (dists '()))
    (let* ((dt (if (number? dt) (seconds->local-time dt) dt))
           (cmd-ls `(,groups
                     ,(nntp-date->string dt)
                     ,(nntp-time->string dt)
                     ,@(if gmt? '("GMT") '())
                     ,@(if (pair? dists)
                           (list (nntp-dists->string dists))
                           '()))))
      (and (apply nntp-send nntp 'NEWGROUPS cmd-ls)
           (make-nntp-data-port 'NEWGROUPS nntp)))))

(define (nntp-newnews-fold nntp kons knil . args)
  (nntp-port-fold (apply nntp-newnews nntp args) kons knil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convenience routine to post a single article in the same style as
;; send-mail from hato-smtp (untested, I don't have a server that
;; accepts posts I can test on)

(define (post-article host-or-nntp group . keys)
  (cond
   ((nntp? host-or-nntp)
    (apply post-article/aux host-or-nntp group keys))
   (else
    (and-let* ((nntp (nntp-connect host-or-nntp)))
      (let ((res (apply post-article/aux nntp group keys)))
        (nntp-disconnect nntp)
        res)))))

(define (post-article/aux nntp group . keys)
  (if group (nntp-group nntp group))
  (and (nntp-post nntp)
       (begin
         (apply smtp-write-message (nntp-out nntp) keys)
         (nntp-get-response nntp))))

