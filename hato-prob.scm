;;;; hato-prob.scm -- classifier probability library
;;
;; Copyright (c) 2005-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; This is where we build up chains of tokens into "features" of the
;; text and compute the combined probability.  The main entry point
;; FEATURE-FOLD takes a huge amount of options in the form of DSSSL
;; keywords, which change at a fast enough rate that I won't bother
;; documenting them yet.

;; There's a lot of premature optimization in the form of fast paths (we
;; don't keep features in memory if we can at all avoid it), usually
;; added while waiting for the comparison results to finish their
;; overnight runs.  Sorry about that.

(require-library posix srfi-69 hato-i3db hato-mime html-parser posix hato-token)

(module hato-prob
  (
   ;; primary api
   feature-fold
   ;; utilities
   deleet i3db-file-name
   ;; mail record
   make-mstats mstats? mstats-words set-mstats-words!
   mstats-urls set-mstats-urls! mstats-ips set-mstats-ips!
   mstats-emails set-mstats-emails! mstats-features set-mstats-features!
   mstats-score set-mstats-score! mstats-count set-mstats-count!
   mstats-prob set-mstats-prob!
   )

(import scheme chicken extras data-structures ports posix srfi-69)
(import html-parser hato-i3db hato-mime hato-token)
(include "write-number.scm")

(define (set-file-position! fd where . o)
  (set! (file-position fd) (if (pair? o) (cons where o) where)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile with "-feature debug" for more debugging info

(cond-expand
 (debug
  (define-syntax debug
    (syntax-rules ()
      ((debug fmt args ...)
       (fprintf (current-error-port) fmt args ...)))))
 (else
  (define-syntax debug (syntax-rules () ((debug fmt args ...) #t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list utils

(define (fold kons knil ls)
  (let lp ((ls ls) (acc knil))
    (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc)))))

(define (filter pred ls)
  (let lp ((ls ls) (res '()))
    (if (null? ls)
      (reverse res)
      (lp (cdr ls) (if (pred (car ls)) (cons (car ls) res) res)))))

(define (unique ls . o)
  (let-optionals* o ((eq equal?) (key identity))
    (map cdr (hash-table->alist
              (alist->hash-table
               (map (lambda (x) (cons (key x) x)) ls) eq)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string utils

(define (pad s i)
  (let* ((s (->string s))
         (diff (fx- i (string-length s))))
    (if (positive? diff)
      (string-append (make-string diff #\space) s)
      s)))
(define (pad-right s i)
  (let* ((s (->string s))
         (diff (fx- i (string-length s))))
    (if (positive? diff)
      (string-append s (make-string diff #\space))
      s)))

(define (string-count s ch)
  (let lp ((i (fx- (string-length s) 1)) (sum 0))
    (cond
      ((fx< i 0) sum)
      ((eq? ch (string-ref s i)) (lp (fx- i 1) (fx+ sum 1)))
      (else (lp (fx- i 1) sum)))))

(define (string-downcase s . o)
  (let-optionals* o ((start 0) (end (string-length s)))
    (let* ((len (fx- end start)) (s2 (make-string len)))
      (let lp ((i start) (j 0))
        (if (fx>= i end)
          s2
          (begin (string-set! s2 j (char-downcase (string-ref s i)))
                 (lp (fx+ i 1) (fx+ j 1))))))))

;; translate 1337-speak
(define (deleet str start end)
  (with-output-to-string
    (lambda ()
      (let ((len (string-length str)))
        (let lp ((i start))
          (when (fx< i end)
            (let ((c (string-ref str i)))
              (if (char-alphabetic? c)
                (display (char-downcase c))
                (case c
                  ((#\4 #\@ #\^) (display #\a))
                  ((#\8) (display #\b))
                  ((#\[ #\< #\() (display #\c))
                  ((#\3 #\&) (display #\e))
                  ((#\6 #\,) (display #\g))
                  ((#\#) (display #\h))
                  ((#\!) (display #\i))
                  ((#\1 #\|) (display #\l))
                  ((#\0) (display #\o))
                  ((#\9) (display #\p))
                  ((#\5) (display #\s))
                  ((#\+ #\7) (display #\t))
                  ((#\%) (display #\y))
                  ((#\2) (display #\z))
                  (else #f))))
            (lp (fx+ i 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; record for holding mail information and statistics

(define-record-type <mstats>
  (%make-mstats words urls ips emails features score inverse-score count prob)
  mstats?
  (words mstats-words set-mstats-words!)
  (urls mstats-urls set-mstats-urls!)
  (ips mstats-ips set-mstats-ips!)
  (emails mstats-emails set-mstats-emails!)
  (features mstats-features set-mstats-features!)
  (score mstats-score set-mstats-score!)
  (inverse-score mstats-inverse-score set-mstats-inverse-score!)
  (count mstats-count set-mstats-count!)
  (prob mstats-prob set-mstats-prob!)
  )

(define (make-mstats . o)
  (let-optionals* o ((words '()) (urls '())
                     (ips '()) (emails '()) (features '())
                     (score 0.0) (inverse-score 0.0) (count 0) (prob 0.0))
    (%make-mstats words urls ips emails features score inverse-score count prob)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; probability utils

(define (chi^2 chi df)
  (debug "chi^2 ~S ~S\n" chi df)
  (if (> df 1000)
    (let ((scale (/ 1000 df)))
      (chi^2 (* chi scale) (* df scale)))
    (let* ((m (/ chi 2.0))
           (e^-m (exp (- m)))
           (limit (quotient df 2)))
      (let lp ((i 1) (term e^-m) (sum e^-m))
        (if (> i limit)
          (min sum 1.0)
          (let ((term (* term (/ m i))))
            (lp (+ i 1) term (+ sum term))))))))

(define (chi^2-combined-prob ls)
  (let lp ((ls ls) (p 1.0) (p-sum '()) (n 0))
    (if (null? ls)
      (chi^2 (* -2 (fold + (log p) p-sum)) (* n 2))
      (let ((p2 (* p (min 0.99 (max 0.01 (car ls))))))
        (if (< p2 1e-200)
          (lp (cdr ls) 1.0 (cons (log p2) p-sum) (+ n 1))
          (lp (cdr ls) p2 p-sum (+ n 1)))))))

(define (chi^2-balanced-prob ls)
  (let ((H (chi^2-combined-prob ls))
        (S (chi^2-combined-prob (map (cut - 1.0 <>) ls))))
    (/ (+ 1 (- H S)) 2)))

(define (mstats-feature-probability a num-significant epsilon naive-bayes?)
  (let ((words (sort (if (positive? epsilon)
                       (filter (lambda (a) (>= (abs (- 0.5 (car a))) epsilon))
                               (mstats-features a))
                       (mstats-features a))
                     (lambda (a b)
                       (> (abs (- 0.5 (car a)))
                          (abs (- 0.5 (car b))))))))
    (let ((sig-words ; take num-significant, plus any words of the same value
           (if (positive? num-significant)
             (let lp ((ls words) (i 0))
               (cond
                 ((null? ls) words)
                 ((fx= i num-significant)
                  (let ((diff (abs (- 0.5 (caar ls)))))
                    (let lp ((ls ls))
                      (cond
                        ((null? (cdr ls)) words)
                        ((= (abs (- 0.5 (caadr ls)))) (lp (cdr ls)))
                        (else (set-cdr! ls '()) words)))))
                 (else (lp (cdr ls) (+ i 1)))))
             words)))
      ;;(debug "sig-words: ~S\n" sig-words)
      (if (null? sig-words)
        0.5
        (if naive-bayes?
          ((lambda  (x)
             (/ (car x) (max 1 (cdr x))))
           (fold (lambda (w a)
                   (let ((weight (cadr w)))
                     (cons (+ (* weight (car w)) (car a))
                           (fx+ weight (cdr a)))))
                 (cons 0.0 0)
                 sig-words))
          (chi^2-balanced-prob (map car sig-words)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chains of tokens form features

(define (token-chain! db chains folders s i j)
  (for-each
   (lambda (vec folder)
     (let* ((s2 (folder s i j))
            (len (string-length s2)))
       (let lp ((k (fx- (vector-length vec) 1)))
         (if (zero? k)
           (vector-set! vec 0 (i3db-hash db s2 0 len (i3db-salt db)))
           (let* ((prev (fx- k 1))
                  (init (i3db-hash db " " 0 1 (vector-ref vec prev))))
             (vector-set! vec k (i3db-hash db s2 0 len init))
             (lp prev))))))
   chains folders))

(define (token-chain-identity! db vec s i j)
  (let lp ((k (fx- (vector-length vec) 1)))
    (if (zero? k)
      (vector-set! vec 0 (i3db-hash db s i j (i3db-salt db)))
      (let* ((prev (fx- k 1))
             (init (i3db-hash db " " 0 1 (vector-ref vec prev))))
        (vector-set! vec k (i3db-hash db s i j init))
        (lp prev)))))

(define (token-chain-with-words! db chains words folders s i j)
  (for-each
   (lambda (vec word-vec folder)
     (let* ((s2 (folder s i j))
            (len (string-length s2)))
       (let lp ((k (fx- (vector-length vec) 1)))
         (cond
           ((zero? k)
            (vector-set! vec 0 (i3db-hash db s2 0 len (i3db-salt db)))
            (vector-set! word-vec 0 s2))
           (else
            (let* ((prev (fx- k 1))
                   (init (i3db-hash db " " 0 1 (vector-ref vec prev)))
                   (prev-word (vector-ref word-vec prev)))
              (vector-set! vec k (i3db-hash db s2 0 len init))
              (vector-set! word-vec k (string-append prev-word " " s2))
              (lp prev)))))))
   chains words folders))

(define (chain-score-bayes db chains weight-factor epsilon feature-prob)
  (let lp1 ((ls chains) (score 0.0) (count 0))
    (if (null? ls)
      (values score 0 count) ; 0 place-holder for unused inverse
      (let* ((vec (car ls))
             (len (vector-length vec)))
        (let lp2 ((i 0) (w 1) (score score) (count count))
          (if (fx= i len)
            (lp1 (cdr ls) score count)
            (receive (s h) (i3db-ref db (vector-ref vec i))
              (let ((ps (feature-prob s h)))
                (if (>= (abs (- ps 0.5)) epsilon)
                  (lp2 (fx+ i 1)
                       (fxshl w weight-factor)
                       (+ score (* w ps))
                       (fx+ count w))
                  (lp2 (fx+ i 1) (fxshl w weight-factor) score count))))))))))

(define (chain-score-chi db chains weight-factor epsilon feature-prob)
  (let lp1 ((ls chains) (s 1.0) (h 1.0) (s-sum '()) (h-sum '()) (count 0))
    (if (null? ls)
      (values (* -2 (fold + (log s) s-sum)) (* -2 (fold + (log h) h-sum)) count)
      (let* ((vec (car ls))
             (len (vector-length vec)))
        (let lp2 ((i 0) (s s) (h h) (s-sum s-sum) (h-sum h-sum) (count count))
          ;;(debug "lp2 ~S ~S ~S ~S\n" s h s-sum h-sum)
          (if (fx= i len)
            (lp1 (cdr ls) s h s-sum h-sum count)
            (receive (num-s num-h) (i3db-ref db (vector-ref vec i))
              (let* ((ps (feature-prob num-s num-h))
                     (ph (- 1.0 ps)))
                (if (>= (abs (- ps 0.5)) epsilon)
                  (let* ((s2 (* s (min 0.99 (max 0.01 ps))))
                         (s3 (if (< s2 1e-200) 1.0 s2))
                         (s-sum2 (if (< s2 1e-200) (cons (log s2) s-sum) s-sum))
                         (h2 (* h (min 0.99 (max 0.01 ph))))
                         (h3 (if (< h2 1e-200) 1.0 h2))
                         (h-sum2 (if (< h2 1e-200) (cons (log h2) h-sum) h-sum)))
                    ;;(debug "s2: ~S s3: ~S h2: ~S h3: ~S\n" s2 s3 h2 h3)
                    (lp2 (fx+ i 1) s3 h3 s-sum2 h-sum2 (fx+ count 1)))
                  (lp2 (fx+ i 1) s h s-sum h-sum count))))))))))

(define (chain-word-score-list db chains words weight-factor feature-prob)
  (let ((last-pair (list #f)))
    (let lp1 ((ls chains) (ls2 words) (res last-pair))
      (if (null? ls)
        (begin
          (set-car! last-pair (car res))
          (cons (cdr res) last-pair))
        (let* ((vec (car ls))
               (word-vec (car ls2))
               (len (vector-length vec)))
          (let lp2 ((i 0) (w 1) (res res))
            (if (fx= i len)
              (lp1 (cdr ls) (cdr ls2) res)
              (let ((key (vector-ref vec i)))
                (receive (s h) (i3db-ref db key)
                  (let ((ps (feature-prob s h)))
                    (lp2 (fx+ i 1)
                         (fxshl w weight-factor)
                         (cons (list ps w (vector-ref word-vec i) s h key) res)
                         )))))))))))

(define (chain-update! db chains spam-offset ham-offset)
  (let lp1 ((ls chains))
    (unless (null? ls)
      (let* ((vec (car ls))
             (len (vector-length vec)))
        (let lp2 ((i 0))
          (if (fx= i len)
            (lp1 (cdr ls))
            (begin
              (i3db-update! db (vector-ref vec i) spam-offset ham-offset)
              (lp2 (fx+ i 1)))))))))

(define (report-features a)
  (define (same-tok? x y) (equal? (caddr x) (caddr y)))
  (let* ((tokens (mstats-features a))
         (spam-tok (map caddr (filter (lambda (x) (= 1.0 (car x))) tokens)))
         (ham-tok (map caddr (filter (lambda (x) (= 0.0 (car x))) tokens)))
         (unknown-tok
          (map caddr
               (filter (lambda (x) (and (zero? (cadddr x)) (zero? (car (cddddr x))))) tokens))))
    (printf "\nPure Spam ~S ~S:\n"
            (length spam-tok)
            (map (lambda (i) (length (filter (lambda (s) (= i (string-count s #\space))) spam-tok)))
                 '(0 1 2 3 4)))
    (for-each (cut printf "    ~S\n" <>) (unique spam-tok))
    (newline)
    (printf "Pure Ham ~S ~S:\n"
            (length ham-tok)
            (map (lambda (i) (length (filter (lambda (s) (= i (string-count s #\space))) ham-tok)))
                 '(0 1 2 3 4)))
    (for-each (cut printf "    ~S\n" <>) (unique ham-tok))
    (newline)
    (printf "Unknown: ~S ~S:\n"
            (length unknown-tok)
            (map (lambda (i) (length (filter (lambda (s) (= i (string-count s #\space))) unknown-tok)))
                 '(0 1 2 3 4)))
    (for-each
     (lambda (x)
       (printf "~A x ~A (~A,~A) ~A ~S\n"
               (pad (cadr x) 4)
               (number->string* (car x) 10 8)
               (pad (cadddr x) 3)
               (pad (car (cddddr x)) 3)
               (pad-right (cadr (cddddr x)) 8)
               (caddr x)))
     (sort
      (unique
       (filter
        (lambda (x) (and (< 0.0 (car x) 1.0) (not (= 0.5 (car x)))))
        tokens)
       same-tok?)
      (lambda (x y) (< (car x) (car y)))))
    (printf "\nchi^2: ~S\n" (chi^2-balanced-prob (map car tokens)))))

(define (i3db-file-name key-size value-size)
  (sprintf "~A/.hato/spam-k~A-v~A.db"
           (or (get-environment-variable "HOME") ".")
           (or key-size "0") value-size))

(define (feature-fold sources
          #!key
          (ham? #f)
          (spam? #f)
          (insert-header? #f)
          (print-result? #f)
          (literal? #f)
          (case-insensitive? #f)
          (deleet? #f)
          (auto-learn? #f)
          (verbose? #f)
          (delete-database? #f)
          (refile? #f)
          (no-update-count? #f)
          (mime? #f)
          (html? #f)
          (proportional-probability? #f)
          (naive-bayes? #f)
          (robinson? #f)
          (db-file #f)
          (offset 1)
          (key-size 2)
          (value-size 2)
          (min-length 1)
          (num-significant 0)
          (chain-length 1)
          (threshold 0.6)  ; bias to reduce false-positives
          (weight-factor 2)
          (epsilon 0.0)
          ;; robinson constants
          (strength 1.0)
          (spam-first 0.5)
          )
  (let* ((write? (or ham? spam? auto-learn?))
         (folders (append (if case-insensitive? (list string-downcase) '())
                          (if deleet? (list deleet) '())
                          (if (or literal?
                                  (not (or case-insensitive? deleet?)))
                            (list substring)
                            '())))
         (db-file (or db-file (i3db-file-name key-size value-size)))
         (db (i3db-open db-file key-size value-size write?))
         (num-spam (i3db-spam db))
         (num-ham (i3db-ham db))
         (s% 0.0)
         (h% 0.0)
         (chains (map (lambda (x) (make-vector chain-length 0)) folders))
         (tokens (map (lambda (x) (make-vector chain-length "")) folders))
         (spam-offset (if spam? offset 0))
         (ham-offset (if ham? offset 0)))
    (letrec
        ((make-knil
          (if naive-bayes?
            make-mstats
            (lambda () (let ((a (make-mstats)))
                    (set-mstats-score! a 1.0)
                    (set-mstats-inverse-score! a 1.0)
                    a))))
         (reset-percentages
          (lambda ()
            (set! s% (if (zero? num-ham) 0.99 (/ num-spam num-ham)))
            (set! h% (if (zero? s%) 0.99 (/ 1.0 s%)))))
         (feature-prob1
          (cond
            (proportional-probability?
             (lambda (s h)
               (let* ((s-p (* s s%))
                      (h-p (* h h%))
                      (divisor (+ s-p h-p)))
                 (if (zero? divisor) 0.5 (/ s-p divisor)))))
            (else
             (lambda (s h)
               (let ((divisor (+ s h)))
                 (if (zero? divisor) 0.5 (/ s divisor)))))))
         (feature-prob
          (if robinson?
            (lambda (s h)
              (/ (+ (* strength spam-first) (* (+ s h) (feature-prob1 s h)))
                 (+ strength s h)))
            feature-prob1))
         (chain-score
          (if naive-bayes? chain-score-bayes chain-score-chi))
         (kons1
          (cond
            ((and (or spam? ham?) (not verbose?) (not (positive? num-significant)))
             (debug "update all\n")
             (if (and (null? (cdr folders)) (eq? (car folders) substring))
               (lambda (s i j a)
                 (token-chain-identity! db (car chains) s i j)
                 (chain-update! db chains spam-offset ham-offset)
                 a)
               (lambda (s i j a)
                 (token-chain! db chains folders s i j)
                 (chain-update! db chains spam-offset ham-offset)
                 a)))
            ((or verbose? ham? spam? auto-learn? (positive? num-significant))
             (debug "using words: verbose? => ~S spam? => ~S ham? => ~S num-significant => ~S\n"
                    verbose? spam? ham? num-significant)
             (lambda (s i j a)
               (token-chain-with-words! db chains tokens folders s i j)
               (let* ((res (chain-word-score-list
                            db chains tokens weight-factor feature-prob))
                      (new (car res))
                      (last-pair (cdr res)))
                 (let ((old (mstats-features a)))
                   (set-cdr! last-pair old)
                   (set-mstats-features! a new))
                 a)))
            (else
             (if (and (null? (cdr folders)) (eq? (car folders) substring))
               (lambda (s i j a)
                 (token-chain-identity! db (car chains) s i j)
                 (receive (ps ph count)
                     (chain-score db chains weight-factor epsilon feature-prob)
                   (set-mstats-score! a (+ (mstats-score a) ps))
                   (set-mstats-inverse-score! a (+ (mstats-inverse-score a) ph))
                   (set-mstats-count! a (fx+ (mstats-count a) count)))
                 a)
               (lambda (s i j a)
                 (token-chain! db chains folders s i j)
                 (receive (ps ph count)
                     (chain-score db chains weight-factor epsilon feature-prob)
                   (set-mstats-score! a (+ (mstats-score a) ps))
                   (set-mstats-inverse-score! a (+ (mstats-inverse-score a) ph))
                   (set-mstats-count! a (fx+ (mstats-count a) count)))
                 a)))))
         (kons
          (if (positive? min-length)
            (lambda (s i j a)
              (if (fx> (fx- j i) min-length)
                (kons1 s i j a)
                a))
            kons1))
         (compute-spam-probability
          (cond
            ((and (or spam? ham?) (not verbose?)
                  (not (positive? num-significant)))
             (lambda (a) (if spam? 1.0 0.0)))
            ((or verbose? spam? ham? auto-learn? (positive? num-significant))
             (lambda (a) (mstats-feature-probability a num-significant epsilon naive-bayes?)))
            (naive-bayes?
             (lambda (a) (/ (mstats-score a) (max 1 (mstats-count a)))))
            (else
             (lambda (a)
               (let* ((count (mstats-count a))
                      (S (chi^2 (mstats-score a) (* 2 count)))
                      (H (chi^2 (mstats-inverse-score a) (* 2 count))))
                 (debug "S: ~S, H: ~S\n" S H)
                 (/ (+ 1 (- S H)) 2))))))
         (final1
          (cond
            ((or ham? spam?)
             (lambda (a)
               (if ham?
                 (set! num-ham (fx+ num-ham 1))
                 (set! num-spam (fx+ num-spam 1)))
               (if (or print-result? (not auto-learn?))
                 (let ((PS (compute-spam-probability a)))
                   (set-mstats-prob! a PS)
                   (when (if ham? (> PS threshold) (<= PS threshold))
                     (for-each  ; false pos/neg, train
                      (lambda (tok)
                        (i3db-update! db (cadr (cddddr tok))
                                      spam-offset ham-offset))
                      (mstats-features a)))
                   (if print-result?
                     (print (if (> PS threshold) "spam " "ham ") PS)))
                 a)))
            (else
             (lambda (a)
               (let ((PS (compute-spam-probability a)))
                 (set-mstats-prob! a PS)
                 (cond
                   ((> PS threshold)
                    (set! num-spam (fx+ num-spam 1))
                    (if refile? (set! num-ham (fx- num-ham 1)))
                    (if print-result? (print "spam " PS)))
                   (else
                    (set! num-ham (fx+ num-ham 1))
                    (if refile? (set! num-spam (fx- num-spam 1)))
                    (if print-result? (print "ham " PS))))
                 a)))))
         (final
          (lambda (a)
            (if verbose? (report-features a))
            (final1 a)))
         (run1
          (if insert-header?
            (cut feature-run-filter reset-percentages threshold kons <>)
            (cut feature-run-non-filter reset-percentages threshold kons <>)))
         (run
          (cond
            ((or mime? html?)
             (lambda ()
               (final
                (mime-message-fold
                 (current-input-port)
                 (mime-headers->list)
                 (lambda (headers str acc)
                   (with-input-from-string
                       (if (and html?
                                (equal? "text/html"
                                        (caar (mime-parse-content-type
                                               (mime-ref headers
                                                         "Content-Type"
                                                         "text/plain")))))
                         (html-strip str)
                         str)
                     (lambda () (run1 acc))))
                 (make-knil)))))
            (else
             (lambda () (final (run1 (make-knil))))))))
      ;; run
      (when verbose?
        (printf "spam?: ~S ham?: ~S auto-learn?: ~S verbose?: ~S\n"
                spam? ham? auto-learn? verbose?)
        (printf "num-spam: ~S num-ham: ~S\n" num-spam num-ham))
      (let ((res
             (if (null? sources)
               (run)
               (let lp ((ls sources) (a (make-knil)))
                 (if (null? ls)
                   a
                   (let ((file (car ls)))
                     (if (directory? file)
                       (let ((r1 (cddr (directory file))))
                         (let ((r2 (map (cut string-append file "/" <>) r1)))
                           (lp (append ls r2) a)))
                       (lp (cdr ls) (with-input-from-file file run)))))))))
        ;; close
        (when db
          (i3db-close db)
          (if (and write? (not no-update-count?))
            (and-let* ((fd (file-open db-file open/rdwr)))
              (set-file-position! fd (* 2 4))
              (write-binary-uint32 num-spam fd)
              (write-binary-uint32 num-ham fd)
              (file-close fd))))
        res))))

(define (make-list-update-kons kons ref set)
  (lambda (s i j a)
    (set a (cons (substring s i j) (ref a)))
    (kons s i j a)))

(define (feature-run-filter reset threshold kons acc)
  (let ((kons-url (make-list-update-kons kons mstats-urls set-mstats-urls!))
        (kons-ip (make-list-update-kons kons mstats-ips set-mstats-ips!))
        (kons-email
         (make-list-update-kons kons mstats-emails set-mstats-emails!)))
    (reset)
    (let lp ((res '()))
      (let ((line (read-line)))
        (cond
          ((eof-object? line)
           acc
           (let lp ((ls (reverse res)))
             (unless (null? ls)
               (cond
                 ((string=? "" (car ls))
                  (print "X-Spam-Classification: "
                         (if (> (mstats-prob acc) threshold) "SPAM " "HAM "))
                  (print "X-Spam-Probability: " (mstats-prob acc))
                  (newline)
                  (for-each print (cdr ls)))
                 (else
                (print (car ls))
                (lp (cdr ls)))))))
          (else
           (token-fold line acc kons kons kons-url kons-ip kons-email)
           (lp (cons line res))))))))

(define (feature-run-non-filter reset threshold kons acc)
  (let ((kons-url (make-list-update-kons kons mstats-urls set-mstats-urls!))
        (kons-ip (make-list-update-kons kons mstats-ips set-mstats-ips!))
        (kons-email
         (make-list-update-kons kons mstats-emails set-mstats-emails!)))
    (reset)
    (let lp ()
      (let ((line (read-line)))
        (cond
          ((eof-object? line)
           acc)
          (else
           (token-fold line acc kons kons kons-url kons-ip kons-email)
           (lp)))))))

)
