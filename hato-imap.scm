;;;; hato-imap.scm -- imap client library
;;
;; Copyright (c) 2008-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Synopsis:
;;
;; (define imap (imap-connect-ssl "imap.gmail.com" "user@gmail.com" "pass"))
;; (imap-select imap 'INBOX)
;; (imap-fetch imap 'UNSEEN '(FLAGS BODY))
;; (imap-fetch-fold imap 'UNSEEN '(FLAGS BODY))
;;
;; Procedures:
;;
;; (imap-connect host username password [debug? port auto-reconnect? ssl?])

;; http://www.faqs.org/rfcs/rfc3501.html
;; http://www.rfc-editor.org/rfc/rfc5092.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-library autoload matchable tcp srfi-1 srfi-13 srfi-18)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module hato-imap
  (
   imap-connect imap-connect-ssl imap-disconnect imap-open
   imap? imap-exists imap-flags imap-resp imap-recent
   imap-login imap-logout imap-send imap-get-response
   imap-select imap-examine imap-status imap-delete
   imap-rename imap-subscribe imap-status->alist
   imap-unsubscribe imap-list imap-lsub imap-append
   imap-check imap-close imap-expunge imap-search imap-search/uid
   imap-fetch imap-fetch/uid imap-fetch-fold imap-fetch-fold/uid
   imap-fetch-one-body imap-fetch-one-body/uid
   imap-store imap-store/uid imap-copy imap-copy/uid)

(import scheme chicken extras matchable data-structures
        srfi-1 srfi-13 srfi-18 posix tcp autoload)

(autoload openssl ssl-connect)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; imap data structure

;; (define-record imap
;;   in out host port user pass debug? auto-connect? ssl?
;;   resp flags exists recent tag)

(define-record-type imap
  (make-imap in out host port user pass debug?
             auto-connect? ssl? resp flags exists recent tag)
  imap?
  (in imap-in imap-in-set!)
  (out imap-out imap-out-set!)
  (host imap-host)
  (port imap-port)
  (user imap-user imap-user-set!)
  (pass imap-pass imap-pass-set!)
  (debug? imap-debug?)
  (auto-connect? imap-auto-connect?)
  (ssl? imap-ssl?)
  (resp imap-resp)
  (flags imap-flags imap-flags-set!)
  (exists imap-exists imap-exists-set!)
  (recent imap-recent imap-recent-set!)
  (tag imap-tag imap-tag-set!))

(define (imap-next-tag imap)
  (let ((i (imap-tag imap)))
    (imap-tag-set! imap (+ i 1))
    (string-append "A"
                   (if (< i 10000)
                       (make-string (if (< i 10) 3 (if (< i 100) 2 1)) #\0)
                       "")
                   (number->string i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; connection and authentication
;;
;; ports: imap - 143 or 220, imaps - 993

(define (imap-connect host user pass . o)
  (let-optionals* o ((debug? #f)
                     (port 220)
                     (auto? #f)
                     (ssl? (eqv? port 993)))
    (let ((imap (imap-open host port debug? auto? ssl?)))
      (cond
       ((not imap)
        #f)
       ((imap-login imap user pass)
        (imap-user-set! imap user)
        (imap-pass-set! imap pass)
        imap)
       (else
        (imap-disconnect imap)
        #f)))))

(define (imap-connect-ssl host user pass . o)
  (let-optionals* o ((debug? #f)
                     (port 993)
                     (auto? #f))
    (imap-connect host user pass debug? port auto? #t)))

(define (imap-disconnect imap)
  (let ((res (or (imap-logout imap)
                 (let ((tag (imap-next-tag imap)))
                   (display tag (imap-out imap))
                   (display " LOGOUT\r\n" (imap-out imap))
                   #f))))
    (close-output-port (imap-out imap))
    (close-input-port (imap-in imap))
    res))

(define (imap-open host . o)
  (let-optionals* o ((port #f)
                     (debug? #f)
                     (auto? #f)
                     (ssl? (eqv? port 993)))
    (let ((port (or port (if ssl? 993 220))))
      (receive (in out)
          (if ssl? (ssl-connect host port) (tcp-connect host port))
        (let* ((imap (make-imap in out host port #f #f
                                debug? auto? ssl? #f '() 0 0 0))
               (res (read-line in))) ;; server welcome line
          imap)))))

(define (port-open? port)
  (and (port? port)
       (not (##sys#slot port 8))))

(define (imap-reconnect imap)
  (let ((host (imap-host imap))
        (port (imap-port imap)))
    (if (imap-debug? imap)
        (fprintf (current-error-port) "imap-reconnecting: ~A@~A:~A~A\n"
                 (imap-user imap) host port (if (imap-ssl? imap) " (ssl)" "")))
    (receive (in out)
        (if (imap-ssl? imap) (ssl-connect host port) (tcp-connect host port))
      (and in
           out
           (begin
             (imap-in-set! imap in)
             (imap-out-set! imap out)
             (imap-login imap (imap-user imap) (imap-pass imap)))))))

(define (imap-reconnect-check imap)
  (or (not (imap-auto-connect? imap))
      (and (port-open? (imap-in imap)) (port-open? (imap-out imap)))
      (imap-reconnect imap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sending messages

(define (imap-number-range? x)
  (and (pair? x)
       (or (number? (car x))
           (eq? '* (car x)))
       (or (number? (cdr x))
           (eq? '* (cdr x)))))

(define (imap-number-set? x)
  (or (number? x)
      (imap-number-range? x)
      (and (pair? x)
           (every (lambda (y)
                    (or (number? y) (imap-number-range? y)))
                  x))))

(define (imap-string-type str)
  (let ((len (string-length str)))
    (if (zero? len)
        'string
        (let lp ((i 0) (res 'symbol))
          (if (>= i len)
              res
              (let* ((c (string-ref str i))
                     (ci (char->integer c)))
                (cond
                 ((or (< ci 32) (> ci 127))
                  'here-doc)
                 ((memv c '(#\space #\" #\# #\% #\* #\( #\) #\{ #\} #\[ #\]))
                  (lp (+ i 1) 'string))
                 (else
                  (lp (+ i 1) res)))))))))

(define imap-month-names
  '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define (imap-date->string tm)
  (let* ((vec (if (vector? tm)
                  tm
                  (seconds->local-time (if (time? tm) (time->seconds tm) tm))))
         (mday (vector-ref vec 3)))
    (string-append
     (if (< mday 10) "0" "") (number->string mday) "-"
     (vector-ref imap-month-names (vector-ref vec 4)) "-"
     (number->string (+ 1900 (vector-ref vec 5))))))

(define (imap-time->string tm)
  (let ((vec (if (vector? tm)
                 tm
                 (seconds->local-time (if (time? tm) (time->seconds tm) tm)))))
    (match vec
      (#(seconds minutes hours mday month year wday yday dstflag timezone)
       (string-append
        (imap-date->string tm) " "
        (if (< hours 10) "0" "")   (number->string hours)   ":"
        (if (< minutes 10) "0" "") (number->string minutes) ":"
        (if (< seconds 10) "0" "") (number->string seconds) " "
        (if (< timezone 0) "-" "+") (if (< (abs timezone) 36000) "0" "")
        (number->string (quotient (abs timezone) 36))))
      (else
       'NIL))))

(define (imap-write obj imap)
  (let ((out (imap-out imap)))
    (cond
     ((pair? obj)
      (cond
       ((imap-number-range? obj)
        (display (string-append (->string (car obj)) ":" (->string (cdr obj)))
                 out))
       ((imap-number-set? obj)
        (display
         (string-intersperse
          (map (lambda (x)
                 (if (pair? x)
                     (string-append (->string (car x)) ":" (->string (cdr x)))
                     (->string x)))
               obj)
          ",")
         out))
       (else
        (write-char #\( out)
        (imap-write (car obj) imap)
        (for-each
         (lambda (x) (write-char #\space out) (imap-write x imap))
         (cdr obj))
        (write-char #\) out))))
     ((vector? obj)
      (let* ((len (vector-length obj))
             (start (cond
                     ((and (> len 0)
                           (memq (vector-ref obj 0) '(BODY body)))
                      (display (vector-ref obj 0) out)
                      1)
                     (else 0))))
        (write-char #\[ out)
        (let lp ((i start))
          (cond
           ((>= i len)
            (write-char #\] out))
           (else
            (if (not (= i start))
                (write-char #\space out))
            (imap-write (vector-ref obj i) imap)
            (lp (+ i 1)))))))
     ((not obj)
      (display "NIL" out))
     ((null? obj)
      (display "()" out))
     ((time? obj)
      (write (imap-time->string obj) out))
     (else
      (let ((str (->string obj)))
        (case (imap-string-type str)
          ((symbol)
           (display str out))
          ((string)
           (write str out))
          (else
           (write-char #\{ out)
           (write (string-length str) out)
           (write-char #\} out)
           (display "\r\n" out)
           (let ((line (read-line (imap-in imap))))
             (if (and (or (not (string? line))
                          (equal? "" line)
                          (not (eqv? #\+ line)))
                      (imap-debug? imap))
                 (fprintf (current-error-port)
                          "imap-write: invalid continuation response ~S\n"
                          line))
             (display obj out)))))))))

(define (imap-send imap cmd . args)
  (let ((tag (imap-next-tag imap)))
    (if (imap-debug? imap)
        (fprintf (current-error-port) "imap-send: ~A ~S ~S\n" tag cmd
                 (if (memq cmd '(LOGIN Login login))
                     (cons (car args) '("********"))
                     args)))
    (let ((out (imap-out imap)))
      (display tag out)
      (write-char #\space out)
      (display cmd out)
      (for-each (lambda (x) (write-char #\space out) (imap-write x imap))
                args)
      (display "\r\n" out)
      tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parsing responses

(define (imap-get-response imap tag kons knil final)
  (let ((in (imap-in imap)))
    (let lp ((knil knil))
      (let ((line (read-line in)))
        (if (imap-debug? imap)
            (fprintf (current-error-port) "imap-resp: ~A\n" line))
        (cond
         ((equal? "" line)
          (fprintf (current-error-port) "blank line in imap response\n"))
         ((eqv? #\* (string-ref line 0))
          (lp (kons imap (cdr (imap-parse-response imap line)) knil)))
         (else
          (let* ((res (string-split line)) ; XXXX not full parse, but
                                           ; we should handle [options]
                 (resp-tag (car res))
                 (ok? (and (pair? (cdr res)) (string-ci=? "OK" (cadr res)))))
            (cond
             ((not (equal? resp-tag tag))
              (fprintf (current-error-port) "got imap tag ~S but expected ~S\n"
                       tag resp-tag)
              (let ((resp-n (string->number (substring resp-tag 1)))
                    (expect-n (string->number (substring tag 1))))
                (if (and (number? resp-n) (number? expect-n)
                         (< resp-n expect-n))
                    (lp knil)
                    (final #f res knil))))
             (else
              (final ok? res knil))))))))))

(define (imap-replace-here-docs imap ls)
  (let ((in (imap-in imap)))
    (let replace ((x ls))
      (cond
       ((pair? x)
        (let ((tail (replace (cdr x))))
          (cons (replace (car x)) tail)))
       ((procedure? x)
        (read-string (x) in))
       (else
        x)))))

(define (string->imap-range str)
  (let ((ls (string-split str ":")))
    (and (pair? ls) (pair? (cdr ls)) (null? (cddr ls))
         (let ((from (or (string->number (car ls))
                         (and (equal? "*" (car ls)) '*)))
               (to (or (string->number (cadr ls))
                       (and (equal? "*" (cadr ls)) '*))))
           (and from to (cons from to))))))

(define (imap-parse-response imap line)
  (let lp ((line line) (len (string-length line)) (i 0) (res '()) (stack '()))
    (if (>= i len)
        (if (null? stack)
            (reverse (imap-replace-here-docs imap res))
            (let* ((stack (imap-replace-here-docs imap (cons res stack)))
                   (line (read-line (imap-in imap))))
              (lp line (string-length line) 0 (car stack) (cdr stack))))
        (case (string-ref line i)
          ((#\")
           (let lp2 ((j (+ i 1)) (ls '()))
             (if (or (>= j len) (eqv? #\" (string-ref line j)))
                 (let ((str (list->string (reverse ls))))
                   (lp line len (+ j 1) (cons str res) stack))
              (let ((c (string-ref line j)))
                (if (eqv? c #\\)
                    (lp2 (+ j 2) (cons (string-ref line (+ j 1)) ls))
                    (lp2 (+ j 1) (cons c ls)))))))
          ((#\()
           (lp line len (+ i 1) '() (cons res stack)))
          ((#\))
           (lp line len (+ i 1) (cons (reverse res) (car stack)) (cdr stack)))
          ((#\{)
           (do ((j (+ i 1) (+ j 1)))
               ((or (>= j len) (eqv? #\} (string-ref line j)))
                (let ((n (string->number (substring line (+ i 1) j))))
                  (if n
                      (lp line len (+ j 1) (cons (lambda () n) res) stack)
                      (error "invalid number in imap literal" line))))))
          ((#\[)
           (do ((j (+ i 1) (+ j 1)))
               ((or (>= j len) (eqv? #\] (string-ref line j)))
                (let ((s (vector (string->symbol (substring line (+ i 1) j)))))
                  (lp line len (+ j 1) (cons s res) stack)))))
          ((#\space #\tab)
           (lp line len (+ i 1) res stack))
          (else
           (do ((j (+ i 1) (+ j 1)))
               ((or (>= j len)
                    (memv (string-ref line j)
                          '(#\space #\tab #\" #\( #\) #\{ #\})))
                (let* ((str (substring line i j))
                       (val0 (or (string->number str)
                                 (string->imap-range str)
                                 (string->symbol (string-upcase str))))
                       (val (if (eq? 'NIL val0) #f val0)))
                  (lp line len j (cons val res) stack)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; building response handlers

(define imap-reconnect-limit 1)

(define (imap-reconnect-wrapper proc)
  (lambda (imap . args)
    (if (not (imap-auto-connect? imap))
        (apply proc imap args)
        ;; try the command normally, if there's an I/O error and
        ;; either of the imap ports have been closed we reconnect and
        ;; try again
        (let lp ((attempts 0))
          (imap-reconnect-check imap)
          (condition-case (apply proc imap args)
            (var (exn i/o)
             (if (not (and (port-open? (imap-in imap))
                           (port-open? (imap-out imap))))
                 (and (< attempts imap-reconnect-limit)
                      (imap-reconnect imap)
                      (lp (+ 1 attempts)))
                 (signal var))))))))

(define (imap-process-flags imap resp acc)
  (cond
   ((and (pair? resp) (number? (car resp))
         (pair? (cdr resp)) (symbol? (cadr resp)))
    (case (cadr resp)
      ((EXISTS) (imap-exists-set! imap (car resp)))
      ((RECENT) (imap-recent-set! imap (car resp)))))
   ((and (pair? resp) (eq? 'FLAGS (car resp)))
    (imap-flags-set! imap (cdr resp))))
  acc)

;; just test for ok
(define imap-send-simple
  (imap-reconnect-wrapper
   (lambda (imap . args)
     (imap-get-response
      imap
      (apply imap-send imap args)
      imap-process-flags
      #f
      (lambda (ok? res acc) ok?)))))

;; expect a tagged list
(define (make-imap-list-processor cmd)
  (lambda (imap resp acc)
    (if (and (pair? resp) (eq? cmd (car resp)))
        (cons (cdr resp) acc)
        (imap-process-flags imap resp acc))))

(define imap-send-list
  (imap-reconnect-wrapper
   (lambda (imap cmd . args)
     (imap-get-response
      imap
      (apply imap-send imap cmd args)
      (make-imap-list-processor cmd)
      '()
      (lambda (ok? res acc)
        (and ok? (reverse acc)))))))

(define imap-send-uid-list
  (imap-reconnect-wrapper
   (lambda (imap uid cmd . args)
     (imap-get-response
      imap
      (apply imap-send imap uid cmd args)
      (make-imap-list-processor cmd)
      '()
      (lambda (ok? res acc)
        (and ok? (reverse acc)))))))

;; expect a FETCH-style list with indexes
(define (make-imap-index-list-processor cmd)
  (lambda (imap resp acc)
    (if (and (pair? resp)
             (number? (car resp))
             (pair? (cdr resp))
             (eq? cmd (cadr resp)))
        (cons (cons (car resp) (cddr resp)) acc)
        (imap-process-flags imap resp acc))))

(define imap-send-index-list
  (imap-reconnect-wrapper
   (lambda (imap cmd . args)
     (imap-get-response
      imap
      (apply imap-send imap cmd args)
      (make-imap-index-list-processor cmd)
      '()
      (lambda (ok? res acc)
        (and ok? (reverse acc)))))))

(define imap-send-uid-index-list
  (imap-reconnect-wrapper
   (lambda (imap uid cmd . args)
     (imap-get-response
      imap
      (apply imap-send imap uid cmd args)
      (make-imap-index-list-processor cmd)
      '()
      (lambda (ok? res acc)
        (and ok? (reverse acc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; search parameters

(define (symbol-upcase sym)
  (let* ((str (symbol->string sym))
         (len (string-length str)))
    (let lp ((i 0))
      (cond
       ((= i len)
        sym)
       ((char-lower-case? (string-ref str i))
        (let ((res (make-string len)))
          (do ((j 0 (+ j 1)))
              ((= j i))
            (string-set! res j (string-ref str j)))
          (string-set! res i (char-upcase (string-ref str i)))
          (do ((j (+ i 1) (+ j 1)))
              ((= j len))
            (string-set! res j (char-upcase (string-ref str j))))
          (string->symbol res)))
       (else
        (lp (+ i 1)))))))

(define (imap-search-translate/aux ls env)
  (define (imap-function? x)
    (and (pair? x)
         (symbol? (car x))
         (eq? 'LAMBDA (symbol-upcase (car x)))))
  (define (imap-apply fun args env)
    (imap-search-translate/aux
     (caddr fun) (append (map cons (cadr fun) args) env)))
  (define (imap-lookup sym . o)
    (cond ((assq sym env) => cdr)
          ((pair? o) (car o))
          (else #f)))
  (cond
   ((not (pair? ls))
    ls)
   ((imap-number-set? ls)
    ls)
   ((symbol? (car ls))
    (let ((sym (symbol-upcase (car ls))))
      (case sym
        ((*)
         (if (not (number? (cdr ls)))
             (error "invalid range" ls)
             ls))
        ((OR)
         (let ((rest (imap-search-translate/aux (cdr ls) env)))
           (case (length rest)
             ((0) 'ALL)
             ((1) (car rest))
             (else
              (pair-fold (lambda (x acc) `(OR ,x ,acc))
                         `(OR ,(car (take-right rest 2)) ,(last rest))
                         (drop-right rest 2))))))
        ((AND)
         (let ((rest (imap-search-translate/aux (cdr ls) env)))
           (case (length rest)
             ((0) '(NOT ALL))
             ((1) (car rest))
             (else
              `(NOT
                ,(pair-fold
                  (lambda (x acc) `(OR (NOT ,x) ,acc))
                  `(OR (NOT ,(car (take-right rest 2)))
                       (NOT ,(last rest)))
                  (drop-right rest 2)))))))
        ((NOT)
         `(,(car ls) ,@(imap-search-translate/aux (cdr ls) env)))
        ((IF)
         (if (not (= 3 (length (cdr ls))))
             (error "imap search IF requires 3 clauses" ls)
             ;; (IF TEST PASS FAIL)
             ;; => (OR (AND TEST PASS) (AND (NOT TEST) FAIL))
             (imap-search-translate/aux
              `(OR (AND ,(cadr ls) ,(caddr ls))
                   (AND (NOT ,(cadr ls)),(cadddr ls)))
              env)))
        ((LET)
         (if (not (= 2 (length (cdr ls))))
             (error "imap LET requires 2 arguments")
             (imap-search-translate/aux
              (caddr ls)
              (append (map (lambda (x)
                             (cons (symbol-upcase (car x)) (cadr x)))
                           (cadr ls))
                      env))))
        ((BEFORE ON SENTBEFORE SENTON SENTSINCE SINCE)
         ;; take a single date argument, coerce integer values to dates
         (let* ((dt0 (imap-search-translate/aux (cadr ls) env))
                (dt1 (if (real? dt0) (seconds->time dt0) dt0))
                (dt (if (or (time? dt1) (vector? dt1))
                        (imap-date->string dt1)
                        dt1)))
           `((,sym ,dt)
             ,@(imap-search-translate/aux (cddr ls) env))))
        ((ALL ANSWERED DELETED DRAFT FLAGGED NEW OLD RECENT SEEN
              UNANSWERED UNDELETED UNDRAFT UNFLAGGED UNSEEN)
         ;; no arguments
         `(,sym ,@(imap-search-translate/aux (cdr ls) env)))
        ((BCC BODY CC FROM KEYWORD LARGER SMALLER SUBJET TEXT TO UID UNKEYWORD)
         ;; one argument
         `((,sym ,(cadr ls))
           ,@(imap-search-translate/aux (cddr ls) env)))
        ((HEADER)
         ;; two arguments
         `((,sym ,(cadr ls) ,(caddr ls))
           ,@(imap-search-translate/aux (cdddr ls) env)))
        ((LAMBDA)
         ls)
        (else
         (let ((cell (assq sym env)))
           (cond
            ((not cell)
             (error "unknown imap search keyword" sym))
            ((imap-function? (cdr cell))
             (imap-apply (cdr cell) (cdr ls) env))
            (else
             (imap-search-translate/aux (cons (cdr cell) (cdr ls)) env))))))))
   ((pair? (car ls))
    (let ((op (imap-search-translate/aux (car ls) env)))
      (if (imap-function? op)
          (imap-apply op (cdr ls) env)
          `(,op ,@(imap-search-translate/aux (cdr ls) env)))))
   (else
    `(,(car ls) ,@(imap-search-translate/aux (cdr ls) env)))))

(define (imap-search-adjust-booleans ls)
  (if (not (pair? ls))
      ls
      (if (not (symbol? (car ls)))
          `(,(car ls) ,@(imap-search-adjust-booleans (cdr ls)))
          (let ((sym (symbol-upcase (car ls))))
            (case sym
              ((AND OR)
               (let ((res (imap-search-adjust-booleans (cdr ls))))
                 (if (not (>= (length res) 2))
                     (error "top-level boolean operators require 2 operands"
                            ls)
                     `((,sym ,(car res) ,(cadr res)) ,@(cddr res)))))
              ((NOT)
               (let ((res (imap-search-adjust-booleans (cdr ls))))
                 `((,sym ,(car res)) ,@(cdr res))))
              (else
               `(,(car ls) ,@(imap-search-adjust-booleans (cdr ls)))))))))

(define (imap-strip-double-negative ls)
  (if (and (pair? ls) (pair? (cdr ls)))
      (if (and (eq? 'NOT (car ls)) (eq? 'NOT (cadr ls)))
          (imap-strip-double-negative (cddr ls))
          (cons (car ls) (imap-strip-double-negative (cdr ls))))
      ls))

(define (imap-flatten ls)
  (cond
   ((not (pair? ls))
    ls)
   ((imap-number-set? ls)
    (list ls))
   ((pair? (car ls))
    (append (imap-flatten (car ls)) (imap-flatten (cdr ls))))
   (else
    (cons (car ls) (imap-flatten (cdr ls))))))

(define (imap-search-translate x)
  (imap-strip-double-negative
   (imap-flatten
    (imap-search-translate/aux
     (cond
      ((not (pair? x)) (list x))
      ((and (pair? x)
            (symbol? (car x))
            (memq (symbol-upcase (car x)) '(AND OR NOT)))
       (if (eq? 'AND (symbol-upcase (car x)))
           (cdr x)
           x))
      (else (imap-search-adjust-booleans x)))
     '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; imap commands

(define (imap-login imap user pass)
  (imap-send-simple imap 'LOGIN user pass))
(define (imap-logout imap)
  (imap-send-simple imap 'LOGOUT))

(define (imap-select imap . o)
  (imap-send-simple imap 'SELECT (if (pair? o) (car o) 'INBOX)))
(define (imap-examine imap . o)
  (imap-send-simple imap 'EXAMINE (if (pair? o) (car o) 'INBOX)))

(define (imap-status imap mbox items)
  (imap-send-list imap 'STATUS mbox items))

(define (imap-status->alist imap mbox items)
  (let lp ((ls (append-map cadr (imap-status imap mbox items)))
           (res '()))
    (cond
     ((not (pair? ls))
      (reverse res))
     ((not (pair? (cdr ls)))
      (warning "incomplete response list in status" ls)
      (reverse res))
     (else
      (lp (cddr ls) (cons (cons (car ls) (cadr ls)) res))))))

(define (imap-delete imap mbox)
  (imap-send-simple imap 'DELETE mbox))

(define (imap-rename imap old-name new-name)
  (imap-send-simple imap 'RENAME old-name new-name))

(define (imap-subscribe imap mbox)
  (imap-send-simple imap 'SUBSCRIBE mbox))

(define (imap-unsubscribe imap mbox)
  (imap-send-simple imap 'UNSUBSCRIBE mbox))

(define (imap-list imap . o)
  (let-optionals* o ((reference "")
                     (name ""))
    (imap-send-list imap 'LIST reference name)))

(define (imap-lsub imap . o)
  (let-optionals* o ((reference "")
                     (name ""))
    (imap-send-list imap 'LSUB reference name)))

(define (imap-append imap mbox msg . o)
  (apply imap-send-simple imap 'APPEND mbox (append o (list msg))))

(define (imap-check imap)
  (imap-send-simple imap 'CHECK))

(define (imap-close imap)
  (imap-send-simple imap 'CLOSE))

(define (imap-expunge imap)
  (imap-send-simple imap 'EXPUNGE))

(define (imap-search imap keywords . o)
  (let ((keywords (imap-search-translate keywords)))
    (concatenate
     (if (and (pair? o) (car o))
         (apply imap-send-list imap 'SEARCH (vector 'CHARSET (car o)) keywords)
         (apply imap-send-list imap 'SEARCH keywords)))))

(define (imap-search/uid imap keywords . o)
  (let ((keywords (imap-search-translate keywords)))
    (concatenate
     (if (and (pair? o) (car o))
         (apply imap-send-uid-list imap 'UID 'SEARCH
                (vector 'CHARSET (car o)) keywords)
         (apply imap-send-uid-list imap 'UID 'SEARCH keywords)))))

(define (imap-message-set imap x)
  (if (imap-number-set? x)
      x
      (imap-search imap (imap-search-translate x))))

(define (imap-message-set/uid imap x)
  (if (imap-number-set? x)
      x
      (imap-search/uid imap (imap-search-translate x))))

(define (imap-fetch imap what items)
  (let ((set (imap-message-set imap what)))
    (if (or (not set) (null? set))
        '()
        (imap-send-index-list imap 'FETCH set items))))

(define (imap-fetch/uid imap what items)
  (let ((set (imap-message-set/uid imap what)))
    (if (or (not set) (null? set))
        '()
        (imap-send-uid-index-list imap 'UID 'FETCH set items))))

(define (imap-get-response-body resp)
  (match resp
    ;; <n> FETCH (|BODY| <str>)
    ((_ _ (data ...))
     (cond ((or (memq (string->symbol "BODY[]") data)
                (memq 'BODY data))
            => cadr)))
    (else #f)))

(define imap-fetch-fold
  (imap-reconnect-wrapper
   (lambda (imap what kons . o)
     (let ((set (imap-message-set imap what))
           (knil (if (pair? o) (car o) '())))
       (if (or (not set) (null? set))
           knil
           (imap-get-response
            imap
            (imap-send imap 'FETCH set '(#(BODY)))
            (lambda (imap resp acc)
              (let ((str (imap-get-response-body resp)))
                (cond
                 ((string? str)
                  (kons (car resp) str acc))
                 (else
                  (fprintf (current-error-port)
                           "imap: couldn't find BODY[] in response: ~S"
                           resp)
                  acc)))) 
            knil
            (lambda (ok? res acc)
              (and ok? acc))))))))

(define imap-fetch-fold/uid
  (imap-reconnect-wrapper
   (lambda (imap what kons . o)
     (fprintf (current-error-port) "imap-fetch-fold/uid ~S\n" what)
     (let ((set (imap-message-set/uid imap what))
           (knil (if (pair? o) (car o) '())))
       (fprintf (current-error-port) "knil: ~S\n" knil)
       (if (or (not set) (null? set))
           knil
           (imap-get-response
            imap
            (imap-send imap 'UID 'FETCH set '(#(BODY)))
            (lambda (imap resp acc)
              (let ((str (imap-get-response-body resp)))
                (cond
                 ((string? str)
                  (kons (car resp) str acc))
                 (else
                  (fprintf (current-error-port)
                           "imap: couldn't find BODY[] in response: ~S"
                           resp)
                  acc)))) 
            knil
            (lambda (ok? res acc)
              (and ok? acc))))))))

(define (imap-fetch-one-body imap message)
  (let ((res (imap-fetch-fold
              imap message (lambda (id text res) (cons text res)) '())))
    (and (pair? res)
         (car res))))

(define (imap-fetch-one-body/uid imap message)
  (let ((res (imap-fetch-fold/uid
              imap message (lambda (id text res) (cons text res)) '())))
    (and (pair? res)
         (car res))))

(define (imap-store imap what name value)
  (let ((set (imap-message-set imap what)))
    (if (or (not set) (null? set))
        #f
        (imap-send-simple imap 'STORE set name value))))

(define (imap-store/uid imap what name value)
  (let ((set (imap-message-set/uid imap what)))
    (if (or (not set) (null? set))
        #f
        (imap-send-simple imap 'UID 'STORE set name value))))

(define (imap-copy imap what mbox)
  (let ((set (imap-message-set imap what)))
    (if (or (not set) (null? set))
        #f
        (imap-send-simple imap 'COPY set mbox))))

(define (imap-copy/uid imap what mbox)
  (let ((set (imap-message-set/uid imap what)))
    (if (or (not set) (null? set))
        #f
        (imap-send-simple imap 'UID 'COPY set mbox))))

)
