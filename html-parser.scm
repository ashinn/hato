;;;; html-parser.scm -- SSAX-like tree-folding html parser
;;
;; Copyright (c) 2003-2008 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; This is intended as a permissive HTML parser for people who prefer
;; the scalable interface described in Oleg Kiselyov's SSAX parser, as
;; well as providing simple convenience utilities.  It correctly
;; handles all invalid HTML, inserting "virtual" starting and closing
;; tags as needed to maintain the proper tree structure needed for the
;; foldts down/up logic.  A major goal of this parser is bug-for-bug
;; compatibility with the way common web browsers parse HTML.

;; Procedure: make-html-parser . keys

;;   Returns a procedure of two arguments, and initial seed and an
;;   optional input port, which parses the HTML document from the port
;;   with the callbacks specified in the plist KEYS (using normal,
;;   quoted symbols, for portability and to avoid making this a
;;   macro).  The following callbacks are recognized:
;;
;;   START: TAG ATTRS SEED VIRTUAL?
;;       fdown in foldts, called when a start-tag is encountered.
;;     TAG:         tag name
;;     ATTRS:       tag attributes as a alist
;;     SEED:        current seed value
;;     VIRTUAL?:    #t iff this start tag was inserted to fix the HTML tree
;;
;;   END: TAG ATTRS PARENT-SEED SEED VIRTUAL?
;;       fup in foldts, called when an end-tag is encountered.
;;     TAG:         tag name
;;     ATTRS:       tag attributes of the corresponding start tag
;;     PARENT-SEED: parent seed value (i.e. seed passed to the start tag)
;;     SEED:        current seed value
;;     VIRTUAL?:    #t iff this end tag was inserted to fix the HTML tree
;;
;;   TEXT: TEXT SEED
;;       fhere in foldts, called when any text is encountered.  May be
;;       called multiple times between a start and end tag, so you need
;;       to string-append yourself if desired.
;;     TEXT:        entity-decoded text
;;     SEED:        current seed value
;;
;;   COMMENT: TEXT SEED
;;       fhere on comment data
;;
;;   DECL: NAME ATTRS SEED
;;       fhere on declaration data
;;       
;;   PROCESS: LIST SEED
;;       fhere on process-instruction data
;;
;;   In addition, entity-mappings may be overriden with the ENTITIES:
;;   keyword.

;; Procedure: html->sxml [port]
;;   Returns the SXML representation of the document from PORT, using
;;   the default parsing options.

;; Procedure: html-strip [port]
;;   Returns a string representation of the document from PORT with all
;;   tags removed.  No whitespace reduction or other rendering is done.

;; Example:
;;
;;   The parser for html-strip could be defined as:
;;
;; (make-html-parser
;;   'start: (lambda (tag attrs seed virtual?) seed)
;;   'end:   (lambda (tag attrs parent-seed seed virtual?) seed)
;;   'text:  (lambda (text seed) (display text)))
;;
;;   Also see the parser for html->sxml.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from SRFI-13

(define (string-downcase str)
  (let lp ((i (- (string-length str) 1)) (res '()))
    (if (negative? i)
        (list->string res)
        (lp (- i 1) (cons (char-downcase (string-ref str i)) res)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SRFI-6 extension if not defined

(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (get-output-string out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text parsing utils

(define (read-while pred . o)
  (let ((in (if (pair? o) (car o) (current-input-port))))
    (call-with-output-string
     (lambda (out)
       (let loop ()
         (let ((c (peek-char in)))
           (cond
            ((and (not (eof-object? c)) (pred c))
             (write-char (read-char in) out)
             (loop)))))))))

(define (read-until pred . o)
  (let ((in (if (pair? o) (car o) (current-input-port))))
    (call-with-output-string
     (lambda (out)
       (let loop ()
         (let ((c (peek-char in)))
           (cond
            ((not (or (eof-object? c) (pred c)))
             (write-char (read-char in) out)
             (loop)))))))))

;; XXXX doesn't account for when the middle of the string can be a
;; prefix of the string (not needed in the uses below)
(define (read-until-string/ci str . o)
  (let ((in (if (pair? o) (car o) (current-input-port)))
        (len (string-length str)))
    (call-with-output-string
     (lambda (out)
       (let loop ((i 0))
         (let ((c (read-char in)))
           (cond
            ((eof-object? c)
             (display (substring str 0 i) out))
            ((char-ci=? c (string-ref str i))
             (if (< i (- len 1))
                 (loop (+ i 1))))
            (else
             (display (substring str 0 i) out)
             (write-char c out)
             (loop 0)))))))))

;; simple utility to look for patterns of the form "aab", reads the
;; whole port if the pattern doesn't occur
(define (read-until-aab a b . o)
  (let ((in (if (pair? o) (car o) (current-input-port))))
    (call-with-output-string
     (lambda (out)
       (let scan ()
         (let ((ch (read-char in)))
           (cond
            ((eof-object? ch))
            ((not (eqv? ch a))
             (write-char ch out)
             (scan))
            (else ;; scanned one a
             (let ((ch (read-char in)))
               (cond
                ((not (eqv? ch a))
                 (write-char a out)
                 (cond ((not (eof-object? ch))
                        (write-char ch out)
                        (scan))))
                (else ;; scanned two a's
                 (let two-a-s ()
                   (let ((ch (read-char in)))
                     (cond ((not (eqv? ch b))
                            (write-char a out)
                            (cond ((eqv? ch a)
                                   (two-a-s))
                                  ((eof-object? ch)
                                   (write-char a out))
                                  (else
                                   (write-char a out)
                                   (write-char ch out)
                                   (scan))))))))))))))))))

(define skip-whitespace (lambda x (apply read-while char-whitespace? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-specific readers

(define (char-alphanumeric? c)
  (or (char-alphabetic? c) (char-numeric? c)))

(define (char-hex-numeric? c)
  (or (char-numeric? c)
      (memv (char-downcase c) '(#\a #\b #\c #\d #\e #\f))))

(define read-identifier (lambda x (apply read-while char-alphanumeric? x)))

(define read-integer (lambda x (apply read-while char-numeric? x)))

(define read-hex-integer (lambda x (apply read-while char-hex-numeric? x)))

(define (read-quoted in)
  (let* ((terminator (read-char in))
         (res (read-until (lambda (c) (eqv? c terminator)) in)))
    (read-char in)
    res))

(define (read-name-or-quoted in)
  (cond ((or (eqv? #\" (peek-char in)) (eqv? #\' (peek-char in)))
         (read-quoted in))
        (else
         (read-while tag-char? in))))

(define (read-pi in)
  (let ((tag (read-identifier in)))
    (skip-whitespace in)
    (list
     (if (equal? tag "") #f (string->symbol (string-downcase tag)))
     (list->string
      (reverse
       (let loop ((res '()))
         (let ((c (peek-char in)))
           (cond
            ((eof-object? c)
             (read-char in)
             res)
            ((eqv? c #\?)
             (read-char in)
             (let loop2 ((res res))
               (cond
                ((eof-object? (peek-char in))
                 (cons #\? res))
                ((eqv? #\> (peek-char in))
                 (read-char in)
                 res)
                ((eqv? #\? (peek-char in))
                 (read-char in)
                 (loop2 (cons c res)))
                (else
                 (loop (cons c res))))))
            (else
             (read-char in)
             (loop (cons c res)))))))))))

(define (read-comment . o)
  (read-until-aab #\- #\> (if (pair? o) (car o) (current-input-port))))

(define (tag-char? c)
  (and (char? c)
       (or (char-alphanumeric? c) (memv c '(#\- #\+ #\* #\_ #\:)))))

(define (read-attrs in)
  (let loop ((attrs '()))
    (skip-whitespace in)
    (let ((c (peek-char in)))
      (cond
       ((or (eof-object? c) (eqv? c #\>))
        (read-char in)
        (list #f (reverse attrs)))
       ((eqv? c #\/)
        (read-char in)
        (skip-whitespace in)
        (cond
         ((eqv? #\> (peek-char in))
          (read-char in)
          (list #t (reverse attrs)))
         (else
          (loop attrs))))
       ((eqv? c #\")
        (read-char in)
        (loop attrs))
       ((not (tag-char? c))
        (list #f (reverse attrs)))
       (else
        (let ((name (read-while tag-char? in)))
          (if (string=? name "")
              (loop attrs)
              (let ((name (string->symbol (string-downcase name))))
                (cond
                 ((eqv? (peek-char in) #\=)
                  (read-char in)
                  (let ((value (if (memv (peek-char in) '(#\" #\'))
                                   (read-quoted in)
                                   (read-until
                                    (lambda (c)
                                      (or (char-whitespace? c)
                                          (memv c '(#\' #\" #\< #\>))))
                                    in))))
                    (if (or (eqv? #\" (peek-char in))
                            (eqv? #\' (peek-char in)))
                        (read-char in))
                    (loop (cons (list name value) attrs))))
                 (else
                  (loop (cons (list name) attrs))))))))))))

(define (read-start in)
  (let ((tag (string->symbol (string-downcase (read-while tag-char? in)))))
    (cons tag (read-attrs in))))

(define (read-end in)
  (let ((tag (read-while tag-char? in)))
    (cond
     ((equal? tag "")
      (read-until (lambda (c) (eqv? c #\>)) in)
      (read-char in)
      #f)
     (else
      (read-attrs in)
      (string->symbol (string-downcase tag))))))

(define (read-decl in)
  (let loop ((res '()))
    (skip-whitespace in)
    (let ((c (peek-char in)))
      (cond
       ((eof-object? c)
        (reverse res))
       ((eqv? c #\>)
        (read-char in)
        (reverse res))
       ((eqv? c #\")
        (loop (cons (read-quoted in) res)))
       ((tag-char? c)
        (loop (cons (string->symbol (read-while tag-char? in)) res)))
       (else
        (loop res))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the parser

(define *default-entities*
  '(("amp" . "&") ("quot" . "\"") ("lt" . "<")
    ("gt" . ">")  ("apos" . "'")  ("nbsp" . " ")))

(define (get-entity entities name)
  (cond
   ((string->number name) => (lambda (n) (string (integer->char n))))
   ((assoc name entities) => cdr)
   (else #f)))

;; span's and div's can be used at any level
(define *tag-levels*
  '(html (head body) table (thead tbody) tr (th td) p (b i u s)))

(define *unnestables*
  '(p li td tr))

(define *bodyless*
  '(img hr br))

(define *literals*
  '(script xmp))

(define *terminators*
  '(plaintext))

(define (tag-level tag-levels tag)
  (let lp ((ls tag-levels) (i 0))
    (if (null? ls)
        (+ i 1000)
        (if (if (pair? (car ls))
                (memq tag (car ls))
                (eq? tag (car ls)))
            i
            (lp (cdr ls) (+ i 1))))))

(define (read-html-token . o)
  (let ((in (if (pair? o) (car o) (current-input-port))))
    (let ((c (peek-char in)))
      (if (eof-object? c)
          (cons 'eof c)
          (case c
            ((#\<)
             (read-char in)
             (case (peek-char in)
               ((#\!)
                (read-char in)
                (cond
                 ((eqv? #\[ (peek-char in))
                  (read-char in)
                  (let lp ((check '(#\C #\D #\A #\T #\A #\[))
                           (acc '(#\[ #\! #\<)))
                    (cond
                     ((null? check)
                      (cons 'text (read-until-aab #\] #\> in)))
                     ((let ((c (peek-char in)))
                        (and (not (eof-object? c)) (char-ci=? c (car check))))
                      (lp (cdr check) (cons (read-char in) acc)))
                     (else
                      (cons 'text (list->string (reverse acc)))))))
                 ((and (eqv? #\- (peek-char in))
                       (begin (read-char in)
                              (eqv? #\- (peek-char in))))
                  (read-char in)
                  (cons 'comment (read-comment in)))
                 (else
                  (cons 'decl (read-decl in)))))
               ((#\?)
                (read-char in)
                (cons 'process (read-pi in)))
               ((#\/)
                (read-char in)
                (cons 'end (read-end in)))
               (else
                ;; start tags must immediately be followed by an
                ;; alphabetic charater, or we just treat the < as text
                (if (and (char? (peek-char in))
                         (char-alphabetic? (peek-char in)))
                    (let ((res (read-start in)))
                      (if (cadr res)
                          (cons 'start/end (cons (car res) (cddr res)))
                          (cons 'start (cons (car res) (cddr res)))))
                    (cons 'text "<")))))
            ((#\&)
             (read-char in)
             (cond
              ((eqv? (peek-char in) #\#)
               (read-char in)
               (cond
                ((char-numeric? (peek-char in))
                 (let* ((str (read-integer in))
                        (num (string->number str)))
                   (if (eqv? (peek-char in) #\;)
                       (read-char in))
                   (cons 'entity num)))
                ((memv (peek-char in) '(#\x #\X))
                 (read-char in)
                 (let* ((str (read-hex-integer in))
                        (num (string->number str 16)))
                   (if (eqv? (peek-char in) #\;)
                       (read-char in))
                   (cons 'entity num)))
                (else
                 (cons 'text "&#"))))
              ((char-alphabetic? (peek-char in))
               (let ((name (read-identifier in)))
                 (if (eqv? (peek-char in) #\;)
                     (read-char in))
                 (cons 'entity name)))
              (else
               (cons 'text "&"))))
            (else
             (cons 'text
                   (read-until (lambda (c) (or (eqv? c #\<) (eqv? c #\&)))
                               in))))))))

(define (%key-ref ls key default)
  (cond ((memq key ls) => cadr) (else default)))

(define (make-html-parser . o)
  (let ((start (%key-ref o 'start: (lambda (t a s v) s)))
        (end (%key-ref o 'end: (lambda (t a p s v) s)))
        (text (%key-ref o 'text: (lambda (t s) s)))
        (decl (%key-ref o 'decl: (lambda (t a s) s)))
        (process (%key-ref o 'process: (lambda (t s) s)))
        (comment (%key-ref o 'comment: (lambda (t s) s)))
        (entities (%key-ref o 'entities: *default-entities*))
        (tag-levels (%key-ref o 'tag-levels: *tag-levels*))
        (unnestables (%key-ref o 'unnestables: *unnestables*))
        (bodyless (%key-ref o 'bodyless: *bodyless*))
        (literals (%key-ref o 'literals: *literals*))
        (terminators (%key-ref o 'terminators: *terminators*))
        (entity (%key-ref o 'entity: #f)))
    (let ((entity (or entity (lambda (t s)
                               (text (if (number? t)
                                         (string (integer->char t))
                                         (or (get-entity entities t)
                                             (string-append "&" t ";")))
                                     s)))))
      (lambda (seed . o)
        (let* ((src (if (pair? o) (car o) (current-input-port)))
               (in (if (string? src) (open-input-string src) src)))
          (let lp ((tok (read-html-token in))
                   (seed seed)
                   (seeds '())
                   (tags '()))
            (case (car tok)
              ((eof)                     ; close all open tags
               (let lp ((t tags) (s seeds) (seed seed))
                 (if (null? t)
                     seed
                     (lp (cdr t) (cdr s)
                         (end (caar t) (cadar t) (car s) seed 'eof)))))
              ((start/end)
               (let ((tag (cadr tok)))
                 (lp `(end . ,tag)
                     (start tag (caddr tok) seed #f)
                     (cons seed seeds)
                     (cons (cdr tok) tags))))
              ((start)
               (let ((tag (cadr tok)))
                 (cond
                  ((memq tag terminators)
                   (lp `(text . ,(read-until (lambda (c) #f) in))
                       (start tag (caddr tok) seed #f)
                       (cons seed seeds)
                       (cons (cdr tok) tags)))
                  ((memq tag literals)
                   (let ((body (read-until-string/ci
                                (string-append "</" (symbol->string tag) ">")
                                in))
                         (seed2 (start tag (caddr tok) seed #f)))
                     (lp `(end . ,tag)
                         (if (equal? "" body) seed2 (text body seed2))
                         (cons seed seeds)
                         (cons (cdr tok) tags))))
                  ((memq tag bodyless)
                   (lp `(end . ,tag)
                       (start tag (caddr tok) seed #f)
                       (cons seed seeds)
                       (cons (cdr tok) tags)))
                  ((and (pair? tags) (eq? tag (caar tags))
                        (memq tag unnestables))
                   ;; <p> ... <p> implies siblings, not nesting
                   (let ((seed2
                          (end tag (cadar tags) (car seeds) seed 'sibling)))
                     (lp (read-html-token in)
                         (start tag (caddr tok) seed #f)
                         (cons seed2 (cdr seeds))
                         (cons (cdr tok) (cdr tags)))))
                  (else
                   (lp (read-html-token in)
                       (start tag (caddr tok) seed #f)
                       (cons seed seeds)
                       (cons (cdr tok) tags))))))
              ((end)
               (cond
                ((not (cdr tok)) ;; nameless closing tag
                 (lp (read-html-token in) seed seeds tags))
                ((and (pair? tags) (eq? (cdr tok) (caar tags)))
                 (lp (read-html-token in)
                     (end (cdr tok) (cadar tags) (car seeds) seed #f)
                     (cdr seeds)
                     (cdr tags)))
                (else
                 (let ((this-level (tag-level tag-levels (cdr tok)))
                       (expected-level
                        (if (pair? tags)
                            (tag-level tag-levels (caar tags))
                            -1)))
                   (cond
                    ((< this-level expected-level)
                     ;; higher-level tag, forcefully close preceding tags
                     (lp tok
                         (end (caar tags) (cadar tags) (car seeds) seed
                              'parent-closed)
                         (cdr seeds)
                         (cdr tags)))
                    ((and (= this-level expected-level) (pair? (cdr tags)))
                     ;; equal, interleave (close prec tag, close this,
                     ;; re-open prec)
                     ;; <b><i></b> => <b><i></i></b><i>
                     ;;                     ^^^^    ^^^
                     ;; XXXX handle backups > 1 here
                     (let* ((seed2 (end (caar tags) (cadar tags)
                                        (car seeds) seed 'interleave))
                            (seed3 (end (caadr tags) (cadadr tags)
                                        (cadr seeds) seed2 #f)))
                       (let ((tok2 (read-html-token in)))
                         (cond
                          ((and (eq? 'end (car tok2))
                                (eq? (caar tags) (cdr tok2)))
                           ;; simple case where the closing tag
                           ;; immediately follows
                           (lp (read-html-token in) seed3
                               (cddr seeds) (cddr tags)))
                          (else
                           (lp tok2
                               (start (caar tags) (cadar tags) seed3
                                      'interleave)
                               (cons seed3 (cddr seeds))
                               (cons (car tags) (cddr tags))))))))
                    (else
                     ;; spurious end for a lower-level tag, add
                     ;; imaginary start
                     (let* ((seed2 (start (cdr tok) '() seed 'no-start))
                            (seed3 (end (cdr tok) '() seed seed2 #f)))
                       (lp (read-html-token in) seed3 seeds tags))))))))
              ((text)
               (lp (read-html-token in) (text (cdr tok) seed) seeds tags))
              ((entity)
               (lp (read-html-token in) (entity (cdr tok) seed) seeds tags))
              ((comment)
               (lp (read-html-token in) (comment (cdr tok) seed) seeds tags))
              ((decl)
               (lp (read-html-token in)
                   (decl (cadr tok) (cddr tok) seed) seeds tags))
              ((process)
               (lp (read-html-token in) (process (cdr tok) seed) seeds tags))
              (else
               (error "invalid token: " tok)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple conversions

(define html->sxml
  (let ((parse
         (make-html-parser
          'start: (lambda (tag attrs seed virtual?) '())
          'end:   (lambda (tag attrs parent-seed seed virtual?)
                    `((,tag ,@(if (pair? attrs)
                                  `((@ ,@attrs) ,@(reverse seed))
                                  (reverse seed)))
                      ,@parent-seed))
          'decl:    (lambda (tag attrs seed) `((*DECL* ,tag ,@attrs) ,@seed))
          'process: (lambda (attrs seed) `((*PI* ,@attrs) ,@seed))
          'comment: (lambda (text seed) `((*COMMENT* ,text) ,@seed))
          'text:    (lambda (text seed) (cons text seed))
          )))
    (lambda o
      (reverse (apply parse '() o)))))

;; XXXX
(define (html-escape-attr str)
  str)

(define (html-attr->string attr)
  (string-append (symbol->string (car attr))
                 "=\"" (html-escape-attr (cadr attr)) "\""))

(define (html-tag->string tag attrs)
  (let lp ((ls attrs) (res (list (symbol->string tag) "<")))
    (if (null? ls)
        (apply string-append (reverse (cons ">" res)))
        (lp (cdr ls) (cons (html-attr->string (car ls)) (cons " " res))))))

(define (html-display-escaped-string str out)
  (let ((start 0)
        (end (string-length str)))
    (let lp ((from start) (to start))
      (if (>= to end)
          (display (substring str from to) out)
          (let ((c (string-ref str to)))
            (cond
             ((eq? c #\<)
              (display (substring str from to) out)
              (display "&lt;" out)
              (let ((next (+ to 1)))
                (lp next next)))
             ((eq? c #\&)
              (display (substring str from to) out)
              (display "&amp;" out)
              (let ((next (+ to 1)))
                (lp next next)))
             (else
              (lp from (+ to 1)))))))))

(define (html-escape str)
  (call-with-output-string
    (lambda (out) (html-display-escaped-string str out))))

(define (sxml-display-as-html sxml . o)
  (let ((out (if (pair? o) (car o) (current-output-port))))
    (cond
     ((pair? sxml)
      (let ((tag (car sxml)))
        (if (symbol? tag)
            (let ((rest (cdr sxml)))
              (cond
               ((and (pair? rest)
                     (pair? (car rest))
                     (eq? '@ (caar rest)))
                (display (html-tag->string tag (cdar rest)) out)
                (for-each (lambda (x) (sxml-display-as-html x out)) (cdr rest))
                (display "</" out) (display tag out) (display ">" out))
               (else
                (display (html-tag->string tag '()) out)
                (for-each (lambda (x) (sxml-display-as-html x out)) rest)
                (display "</" out) (display tag out) (display ">" out))))
            (for-each (lambda (x) (sxml-display-as-html x out)) sxml))))
     ((null? sxml))
     (else (html-display-escaped-string sxml out)))))

(define (sxml->html sxml . o)
  (call-with-output-string
    (lambda (out) (sxml-display-as-html sxml out))))

;; just strips tags, no whitespace handling or formatting
(define (html-strip . o)
  (call-with-output-string
   (lambda (out)
     (let ((parse
            (make-html-parser
             'start: (lambda (tag attrs seed virtual?) seed)
             'end:   (lambda (tag attrs parent-seed seed virtual?) seed)
             'text:  (lambda (text seed) (display text out)))))
       (apply parse (cons #f #f) o)))))
