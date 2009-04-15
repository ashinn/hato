;;;; hato-pop.scm -- pop3 client library
;;
;; Copyright (c) 2003-2008 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; Easy to use POP3 (RFC 1939) library, with support for APOP and SSL,
;; efficient accessing of messages as virtual input ports, and safe
;; locking of other operations while those input ports are overlayed.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use autoload tcp extras utils srfi-1 hato-mime)
(autoload openssl ssl-connect)
(autoload md5 md5:digest)

(define-record-type pop3
  (make-pop3 in out debug? timestamp resp locked)
  pop3?
  (in pop3-in)
  (out pop3-out)
  (debug? pop3-debug?)
  (timestamp pop3-timestamp pop3-timestamp-set!)
  (resp pop3-resp pop3-resp-set!)
  (locked pop3-locked pop3-locked-set!)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond-expand
 ((and chicken compiling)
  (declare
   (export pop3-connect pop3-disconnect pop3-open pop3-open-ssl pop3?
           pop3-apop pop3-auth pop3-dele pop3-disconnect
           pop3-list pop3-noop pop3-open pop3-pass pop3-quit
           pop3-retr pop3-rset pop3-stat pop3-top pop3-uidl pop3-user
           pop3-resp pop3-retr->string pop3-top->string pop3-headers)))
 (else))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chicken's pop3 egg compatibility

(define (pop3-connect host user pass . o)
  (let ((debug? (and (pair? o) (car o)))
        (port (or (and (pair? o) (pair? (cdr o)) (cadr o)) 110)))
    (let ((pop (pop3-open host port debug?)))
      (cond
       ((not pop)
        #f)
       ((or (pop3-apop pop user pass)
            (pop3-auth pop user pass))
        pop)
       (else
        (pop3-disconnect pop)
        #f)))))

(define (pop3-disconnect pop)
  (let ((res (or (pop3-quit pop)
                 (begin (display "QUIT\r\n" (pop3-out pop)) #f))))
    (close-output-port (pop3-out pop))
    (close-input-port (pop3-in pop))
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; connection and authentication

;; pop3 - 110, pop3s - 995

(define (pop3-open-internal host port ssl? debug?)
  (receive (in out) (if ssl? (ssl-connect host port) (tcp-connect host port))
    (let* ((pop (make-pop3 in out debug? #f #f #f))
           (res (pop3-get-response pop)))
      (if (and res (string? (pop3-resp pop)))
          (let ((msg (last (string-split (pop3-resp pop)))))
            (if (and (> (string-length msg) 2)
                     (eqv? #\< (string-ref msg 0))
                     (eqv? #\> (string-ref msg (- (string-length msg) 1))))
                (pop3-timestamp-set! pop msg))))
      pop)))

(define (pop3-open host . o)
  (let ((port (if (pair? o) (car o) 110))
        (debug? (and (pair? o) (pair? (cdr o)) (cadr o))))
    (pop3-open-internal host port (= port 995) debug?)))

(define (pop3-open-ssl host . o)
  (let ((port (if (pair? o) (car o) 995))
        (debug? (and (pair? o) (pair? (cdr o)) (cadr o))))
    (pop3-open-internal host port #t debug?)))

(define (pop3-send pop cmd . args)
  (if (pop3-debug? pop)
      (fprintf (current-error-port) "pop3-send: ~S ~S\n" cmd
               (if (memq cmd '(PASS Pass pass)) '("********") args)))
  (cond
   ((pop3-locked pop)
    (fprintf
     (current-error-port)
     "pop3-send: warning, can't send '~S ~S' while locked by command ~S\n"
     cmd args (pop3-locked pop))
    #f)
   (else
    (let ((out (pop3-out pop)))
      (display cmd out)
      (for-each (lambda (x) (write-char #\space out) (display x out)) args)
      (newline out)
      (pop3-get-response pop)))))

(define (pop3-get-response pop)
  (let ((line (read-line (pop3-in pop))))
    (if (pop3-debug? pop)
        (fprintf (current-error-port) "pop3-resp: ~A\n" line))
    (if (not (and (> (string-length line) 1)
                  (memv (string-ref line 0) '(#\+ #\-))))
        (error "invalid pop3 response" line)
        (let* ((res (eqv? #\+ (string-ref line 0)))
               (i (substring-index " " line))
               (msg (if i (substring line (+ i 1) (string-length line)) "")))
          (pop3-resp-set! pop msg)
          res))))

(define (pop3-auth pop user pass)
  (and (pop3-send pop 'USER user)
       (pop3-send pop 'PASS pass)))

(define (pop3-apop pop user key)
  (let ((ts (pop3-timestamp pop)))
    (and ts (pop3-send pop 'APOP user (md5:digest (string-append ts key))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; low-level commands

(define (pop3-user pop user) (pop3-send pop 'USER user))
(define (pop3-pass pop pass) (pop3-send pop 'PASS pass))
(define (pop3-dele pop i) (pop3-send pop 'DELE i))
(define (pop3-rset pop) (pop3-send pop 'RSET))
(define (pop3-noop pop) (pop3-send pop 'NOOP))
(define (pop3-quit pop) (pop3-send pop 'QUIT))

(define (pop3-stat pop)
  (and (pop3-send pop 'STAT)
       (map string->number (string-split (pop3-resp pop)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list commands

(define (pop3-read-list cmd pop)
  (let ((in (pop3-in pop)))
    (pop3-locked-set! pop cmd)
    (let lp ((res '()))
      (let ((line (read-line in)))
        (cond
         ((or (eof-object? line) (equal? "." line))
          (pop3-locked-set! pop #f)
          (reverse res))
         ((and (not (equal? "" line)) (eqv? #\. (string-ref line 0)))
          (lp (cons (substring line 1 (string-length line)) res)))
         (else
          (lp (cons line res))))))))

(define (pop3-list pop . o)
  (and (apply pop3-send pop 'LIST o)
       (filter-map (lambda (line)
                     (let ((i (substring-index " " line)))
                       (and i
                            (cons (string->number (substring line 0 i))
                                  (string->number (substring line (+ i 1)))))))
                   (pop3-read-list 'LIST pop))))

(define (pop3-uidl pop . o)
  (and (apply pop3-send pop 'UIDL o)
       (if (pair? o)
           ;; single uid in initial response
           (cadr (string-split (pop3-resp pop)))
           ;; read list
           (filter-map (lambda (line)
                      (let ((i (substring-index " " line)))
                        (and i
                             (cons (string->number (substring line 0 i))
                                   (substring line (+ i 1))))))
                    (pop3-read-list 'UIDL pop)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; retrieving messages

(define (make-pop3-data-port cmd pop)
  (let ((in (pop3-in pop))
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
      (pop3-locked-set! pop #f))
    (pop3-locked-set! pop cmd)
    (make-input-port read-char char-ready? close peek-char)))

(define (pop3-retr pop msg)
  (and (pop3-send pop 'RETR msg)
       (make-pop3-data-port 'RETR pop)))

(define (pop3-top pop msg n)
  (and (pop3-send pop 'TOP msg n)
       (make-pop3-data-port 'TOP pop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with- convenience forms

(define (call-with-pop3-message pop msg proc)
  (let ((port (pop3-retr pop msg)))
    (and port
         (let ((res (condition-case (proc port)
                      (exn ()
                        ;;(print-error-message exn (current-error-port))
                        (close-input-port port)
                        (pop3-locked-set! pop #f)
                        (signal exn)))))
           (close-input-port port)
           res))))

(define (call-with-pop3-top pop msg n proc)
  (let ((port (pop3-top pop msg n)))
    (and port
         (let ((res (condition-case (proc port)
                      (exn ()
                        (close-input-port port)
                        (pop3-locked-set! pop #f)
                        (signal exn)))))
           (close-input-port port)
           res))))

(define (pop3-retr->string pop msg)
  (call-with-pop3-message pop msg read-all))

(define (pop3-top->string pop msg n)
  (call-with-pop3-top pop msg n read-all))

(define (pop3-headers pop msg)
  (call-with-pop3-top pop msg 1 mime-headers->list))

