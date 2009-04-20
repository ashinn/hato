;;;; hato-mime.scm -- RFC2045 MIME library
;;
;; Copyright (c) 2005-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RFC2822 headers

;; Procedure: mime-header-fold kons knil [source kons-from]
;;
;;  Performs a fold operation on the MIME headers of source which can be
;;  either a string or port, and defaults to current-input-port.  kons
;;  is called on the three values:
;;     kons header value accumulator
;;  where accumulator begins with knil.  Neither the header nor the
;;  value are modified, except wrapped lines are handled for the value.
;;
;;  The optional procedure KONS-FROM is a procedure to be called when
;;  the first line of the headers is an "From <address> <date>" line, to
;;  enable this procedure to be used as-is on mbox files and the like.
;;  It defaults to KONS, and if such a line is found the fold will begin
;;  with (KONS-FROM "%from" <address> (KONS-FROM "%date" <date> KNIL)).

;; Procedure: mime-headers->list [source]
;;   Return an alist of the MIME headers from source with headers all
;;   downcased.

;; Procedure: mime-parse-content-type str
;;   Parses STR as a Content-Type style-value returning the list
;;     (type (attr . val) ...)
;;  For example:
;;     (mime-parse-content-type
;;        "text/html; CHARSET=US-ASCII; filename=index.html")
;;       => ("text/html" ("charset" . "US-ASCII") ("filename" . "index.html"))

;; Procedure: mime-decode-header str
;;   Replace all occurrences of RFC1522 =?ENC?...?= escapes in STR with
;;   the appropriate decoded and charset converted value.

;; Procedure: mime-ref headers str [default]
;;   A case-insensitive assoc-ref.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RFC2045 MIME encoding

;; Procedure: mime-message-fold src headers kons knil
;;   Performs a fold operation on the given string or port SRC as a MIME
;;   body corresponding to the headers give in HEADERS.  KONS is called
;;   on the successive values:
;;
;;      KONS part-headers part-body accumulator
;;
;;   where part-headers are the headers for the given MIME part (the
;;   original headers for single-part MIME), part-body is the
;;   appropriately decoded and charset-converted body of the message,
;;   and the accumulator begins with KNIL.
;;
;; TODO: Extend mime-message-fold to (optionally?) pass KONS an
;; input-port instead of string for the body to handle very large bodies
;; (this is not much of an issue for SMTP since the messages are in
;; practice limited, but it could be problematic for large HTTP bodies).
;;
;; This does a depth-first search, folding in sequence.  It should
;; probably be doing a tree-fold as in html-parser.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module hato-mime
  (mime-ref assoc-ref mime-header-fold mime-headers->list
   mime-parse-content-type mime-decode-header
   mime-message-fold)

(import scheme chicken extras hato-base64 quoted-printable charconv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; warn on invalid headers in debug mode

(cond-expand
 (debug
  (define-syntax warn
    (syntax-rules ()
      ((warn args ...) (begin (fprintf (current-error-port) args ...) #t)))))
 (else
  (define-syntax warn (syntax-rules () ((warn args ...) #t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; association lists

(define (assoc* key ls . o)
  (let-optionals* o ((eq equal?))
    (let lp ((ls ls))
      (cond
        ((null? ls) #f)
        ((eq key (caar ls)) (car ls))
        (else (lp (cdr ls)))))))

(define (assoc-ref ls key . o)
  (let-optionals* o ((default #f) (eq equal?))
    (cond ((assoc* key ls eq) => cdr)
          (else default))))

(define (mime-ref ls key . o)
  (assoc-ref ls key (and (pair? o) (car o)) substring-ci=?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple matching instead of regexps

(define (match-mbox-from-line line)
  (let ((len (string-length line)))
    (and (> len 5)
         (substring=? line "From " 0 0 5)
         (let lp ((i 6))
           (cond
             ((= i len) (list (substring line 5) ""))
             ((memq (string-ref line i) '(#\space #\tab))
              (list (substring line 5 i) (substring line (+ i 1))))
             (else (lp (+ i 1))))))))

(define (string-scan-colon-or-maybe-equal str)
  (let ((len (string-length str)))
    (let lp ((i 0) (best #f))
      (if (= i len)
          best
          (let ((c (string-ref str i)))
            (cond ((or (char-alphabetic? c)
                       (char-numeric? c)
                       (memv c '(#\- #\_)))
                   (lp (+ i 1) best))
                  ((eq? c #\:)
                   (if (= i 0) #f i))
                  ((eqv? c #\=)
                   (lp (+ i 1) (or best i)))
                  (else
                   best)))))))

(define (match-mime-header-line line)
  (let ((i (string-scan-colon-or-maybe-equal line)))
    (and i (list (substring line 0 i) (substring line (+ i 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some srfi-13 & string utils

(define (string-copy! to tstart from . o)
  (let-optionals* o ((start 0) (end (string-length from)))
    (let lp ((i start) (j tstart))
      (unless (>= i end)
        (string-set! to j (string-ref from i))
        (lp (+ i 1) (+ j 1))))))

(define (string-concatenate-reverse ls)
  (let lp ((ls ls) (rev '()) (len 0))
    (if (null? ls)
      (let ((res (make-string len)))
        (let lp ((ls rev) (i 0))
          (if (null? ls)
            res
            (begin
              (string-copy! res i (car ls))
              (lp (cdr ls) (+ i (string-length (car ls))))))))
      (lp (cdr ls) (cons (car ls) rev) (+ len (string-length (car ls)))))))

(define (string-downcase s . o)
  (let-optionals* o ((start 0) (end (string-length s)))
    (let* ((len (- end start)) (s2 (make-string len)))
      (let lp ((i start) (j 0))
        (if (>= i end)
          s2
          (begin (string-set! s2 j (char-downcase (string-ref s i)))
                 (lp (+ i 1) (+ j 1))))))))

(define (string-char-index str c . o)
  (let-optionals* o ((start 0) (end (string-length str)))
    (let lp ((i start))
      (cond
        ((= i end) #f)
        ((eq? c (string-ref str i)) i)
        (else (lp (+ i 1)))))))

(define (string-trim-white-space s)
  (let ((len (string-length s)))
    (let lp ((i 0))
      (cond ((= i len) "")
            ((char-whitespace? (string-ref s i)) (lp (+ i 1)))
            (else
             (let lp ((j (- len 1)))
               (cond ((<= j i) (substring s i (+ j 1)))
                     ((char-whitespace? (string-ref s j)) (lp (- j 1)))
                     (else (substring s i (+ j 1))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; header parsing

(define (mime-header-fold kons knil . o)
  (let-optionals* o ((src #f) (kons-from kons))
    ((if (string? src) mime-header-fold-string mime-header-fold-port)
     kons knil (or src (current-input-port)) kons-from)))

(define (mime-header-fold-string kons knil str kons-from)
  (call-with-input-string str
    (cut mime-header-fold-port kons knil <> kons-from)))

(define (mime-header-fold-port kons knil port kons-from)
  (define (out line acc)
    (cond
      ((or (eof-object? line) (string=? line ""))
       acc)
      ((match-mime-header-line line)
       => (lambda (m)
            (in (car m) (list (cadr m)) acc)))
      (else
       (warn "invalid header line: ~S\n" line)
       (out (read-line port) acc))))
  (define (in header value acc)
    (let ((line (read-line port)))
      (cond
        ((or (eof-object? line) (string=? line ""))
         (kons header (string-concatenate-reverse value) acc))
        ((char-whitespace? (string-ref line 0))
         (in header (cons line value) acc))
        (else
         (out line (kons header (string-concatenate-reverse value) acc))))))
  (let ((first-line (read-line port)))
    (cond
      ((eof-object? first-line)
       knil)
      ((and kons-from (match-mbox-from-line first-line))
       => (lambda (m) ; special case check on first line for mbox files
            (out (read-line port)
                 (kons-from "%from" (car m)
                       (kons-from "%date" (cadr m) knil)))))
      (else
       (out first-line knil)))))

(define (mime-headers->list . o)
  (let ((port (if (pair? o) (car o) (current-input-port))))
    (reverse
     (mime-header-fold
      (lambda (h v acc) (cons (cons (string-downcase h) v) acc))
      '()
      port))))

(define (mime-split-name+value s)
  (let ((i (string-char-index s #\=)))
    (if i
      (cons (string-downcase (string-trim-white-space (substring s 0 i)))
            (if (= i (string-length s))
              ""
              (if (eqv? #\" (string-ref s (+ i 1)))
                (substring s (+ i 2) (- (string-length s) 1))
                (substring s (+ i 1)))))
      (cons (string-downcase (string-trim-white-space s)) ""))))

(define (mime-parse-content-type str)
  (map mime-split-name+value (string-split str ";")))

(define (mime-decode-header str)
  (let* ((len (string-length str))
         (limit (- len 8))) ; need at least 8 chars: "=?Q?X??="
    (let lp ((i 0) (from 0) (res '()))
      (if (>= i limit)
        (string-concatenate-reverse (cons (substring str from) res))
        (if (and (eqv? #\= (string-ref str i))
                 (eqv? #\? (string-ref str (+ i 1))))
          (let* ((j (string-char-index str #\? (+ i 3)))
                 (k (string-char-index str #\? (+ j 3))))
            (if (and j k (< (+ k 1) len)
                     (eqv? #\? (string-ref str (+ j 2)))
                     (memq (string-ref str (+ j 1)) '(#\Q #\B #\q #\b))
                     (eqv? #\= (string-ref str (+ k 1))))
              (let ((decode (if (memq (string-ref str (+ j 1)) '(#\Q #\q))
                               quoted-printable-decode-string
                               base64-decode-string))
                    (cset (substring str (+ i 2) j))
                    (content (substring str (+ j 3) k))
                    (k2 (+ k 2)))
                (lp k2 k2 (cons (ces-convert (decode content) cset)
                                (cons (substring str from i) res))))
              (lp (+ i 2) from res)))
          (lp (+ i 1) from res))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; message parsing

(define (mime-read-to-boundary port boundary)
  (let ((done? (if boundary
                   (cut equal? <> boundary)
                   (lambda (s) (and (> (string-length s) 4)
                               (string=? "From " (substring s 0 5)))))))
    (let lp ((res '()))
      (let ((line (read-line port)))
        (if (or (eof-object? line) (done? line))
            (string-intersperse (reverse res)
                                (with-output-to-string newline))
            (lp (cons line res)))))))

(define (mime-read-part port cte enc boundary)
  (let ((str (mime-read-to-boundary port boundary)))
    (cond
      ((substring-ci=? cte "quoted-printable")
       (ces-convert (quoted-printable-decode-string str) enc))
      ((substring-ci=? cte "base64")
       (ces-convert (base64-decode-string str) enc))
      (else
       (ces-convert str enc)))))

;; (kons parent-headers part-headers part-body seed)
;; (start headers seed)
;; (end headers parent-seed seed)
(define (mime-message-fold src kons init-seed . o)
  (let ((port (if (string? src) (open-input-string src) src)))
    (let-optionals* o ((kons-start
                        (lambda (headers seed) '()))
                       (kons-end
                        (lambda (headers parent-seed seed)
                          `((mime (@ ,@headers) ,@(reverse seed))
                            ,@parent-seed)))
                       (headers
                        (if (pair? o) (car o) (mime-headers->list port))))
      (let fold ((parent-headers '())
                 (headers headers)
                 (seed init-seed))
        (let* ((ctype (mime-parse-content-type
                       (mime-ref headers "Content-Type" "text/plain")))
               (type (caar ctype))
               (enc (string-trim-white-space
                     (or (mime-ref ctype "charset")
                         (mime-ref headers "charset" "ASCII"))))
               (cte (string-trim-white-space
                     (or (mime-ref headers "Content-Transfer-Encoding")
                         (mime-ref headers "Encoding" "7-bit")))))
          (cond
           ((and (substring-ci=? type "multipart/")
                 (mime-ref type "boundary"))
            => (lambda (boundary)
                 (mime-read-to-boundary port boundary)
                 (let lp ((part-seed (kons-start headers seed)))
                   (let ((part-headers (mime-headers->list port)))
                     (if (null? part-headers)
                         (kons-end headers seed part-seed)
                         (let ((part (mime-read-part port cte enc boundary)))
                           (lp (fold kons port headers2 acc))))))))
           (else
            (let ((body (mime-read-part port cte enc #f)))
              (kons parent-headers headers body seed)))))))))

;; (mime (@ (header . value) ...) parts ...)
(define (mime-message->sxml . o)
  (mime-message-fold
   (if (pair? o) (car o) (current-input-port))
   (lambda (parent-headers headers body seed) (cons body seed))))

)
