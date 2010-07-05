;;;; hato-date.scm -- time and date utilities
;;
;; Copyright (c) 2008-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-library regex-case)

(module hato-date
  (parse-date wday->number mon->number
   date<=? date>=? date=? time<=? time>=? time=?)

(import scheme chicken extras regex posix data-structures srfi-13 regex-case)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (wday->number x)
  (case (string->symbol
         (string-downcase
          (if (symbol? x) (symbol->string x) x)))
    ((sun) 0)
    ((mon) 1)
    ((tue) 2)
    ((wed) 3)
    ((thu) 4)
    ((fri) 5)
    ((sat) 6)
    (else #f)))

(define (mon->number x)
  (case (string->symbol
         (string-downcase
          (if (symbol? x) (symbol->string x) x)))
    ((jan) 1)
    ((feb) 2)
    ((mar) 3)
    ((apr) 4)
    ((may) 5)
    ((jun) 6)
    ((jul) 7)
    ((aug) 8)
    ((sep) 9)
    ((oct) 10)
    ((nov) 11)
    ((dec) 12)
    (else #f)))

(define time-zone-abbrevs
  `(("CDT" . ,(* -5 60 60))
    ("CST" . ,(* -6 60 60))
    ("EDT" . ,(* -4 60 60))
    ("EST" . ,(* -5 60 60))
    ("PDT" . ,(* -7 60 60))
    ("PST" . ,(* -8 60 60))
    ("NDT" . ,(+ (* -2 60 60) (* -30 60)))
    ("NST" . ,(+ (* -3 60 60) (* -30 60)))
    ("GMT" . 0)
    ("UTC" . 0)
    ))

(define (tz->number x)
  (if (not (string? x))
      0
      (let ((m (string-match "^([-+]?\\d{2})(\\d+)" x)))
        (if m
            (* 60 (+ (* 60 (string->number (cadr m)))
                     (string->number (caddr m))))
            (cond
             ((assoc m time-zone-abbrevs) => cdr)
             (else 0))))))

(define (make-date seconds minutes hours mday month year wday yday dstflag tz)
  (seconds->utc-time
   (- (utc-time->seconds
       (vector seconds minutes hours mday month year wday yday #f 0))
      (or tz 0))))

;; universal, loose parser
(define parse-date
  (let ()
    (define (i x) (or (and (string? x) (string->number x)) 0))
    (define (save . args) (apply string-append "(" (append args '(")"))))
    (define (group . args) (apply string-append "(?:" (append args '(")"))))
    (define (rx-or ls) (save (string-join ls "|")))
    (define (? . args) (string-append (apply group args) "?"))
    (define (abbrev-rx x n)
      (let ((str (->string x)))
        (string-append (string-take str n) (? (string-drop str n)))))
    (define (rx-abbrev-ls ls n)
      (rx-or (map (cut abbrev-rx <> n) ls)))
    (define wday-en '(Sunday Monday Tuesday Wednesday Thursday Friday Saturday))
    (define rx-wday-en (rx-abbrev-ls wday-en 3))
    (define mon-en '(January February March April May June July
                             August September October November December))
    (define rx-mon-en (rx-abbrev-ls mon-en 3))
    (define sep "\\s*[-_,;:/]?\\s*")
    (define d2 "(\\d{1,2})")
    (define d4 "(\\d{1,4})")
    (define rx-tz
      (group (rx-or '("[-+]\\d{2}[013][05]" "\\w+/\\w+"))
             (? "\\s*\\(?[A-Za-z]{3}\\)?\\s*")))
    (define rx-mag
      (regexp
       (string-append
        "^" d4 sep d2 sep d2
        (? (? "T") sep d2 sep d2 sep d2 sep (? sep rx-tz)))))
    (define rx-822
      (regexp
       (string-append (? rx-wday-en sep) d2 sep rx-mon-en sep d4
                      (? sep d2 sep d2 sep d2) (? sep rx-tz))
       #t))
    (define rx-unix
      (regexp
       (string-append (? rx-wday-en sep) rx-mon-en sep d2
                      (? sep d2 sep d2 sep d2) (? sep rx-tz)
                      sep d4)))

    (lambda (str)
      (and
       (string? str)
       (regex-case
        (string-trim str)
        ;; Put the formats with named weekdays/months first since if
        ;; they occur we can be confident of a match.
        ;;
        ;; RFC-822 style (~a, ~d ~b ~Y ~H:~M:~S ~z)
        (rx-822 (_ w d m y h mi ss tz)
                (make-date (i ss) (i mi) (i h) (i d) (- (mon->number m) 1)
                           (- (i y) 1900) 0 0 #f (tz->number tz)))
        ;; Unix style
        (rx-unix (_ w m d h mi ss tz y)
                 (make-date (i ss) (i mi) (i h) (i d) (- (mon->number m) 1)
                            (- (i y) 1900) 0 0 #f (tz->number tz)))
        ;; YYYY-MM-DD style
        (rx-mag (_ y m d h mi ss tz)
                (make-date (i ss) (i mi) (i h) (i d) (- (i m) 1) (- (i y) 1900)
                           0 0 #f (tz->number tz)))
        (else
         ;;(warning "unrecognized date format: ~S" str)
         #f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic date orderings

(define-inline (date-value a)
  (+ (* 10000 (vector-ref a 5))
     (* 100 (vector-ref a 4))
     (vector-ref a 3)))

(define (date<=? a b)
  (let ((a (if (number? a) (seconds->utc-time a) a))
        (b (if (number? b) (seconds->utc-time b) b)))
    (<= (date-value a) (date-value b))))

(define (date>=? a b)
  (let ((a (if (number? a) (seconds->utc-time a) a))
        (b (if (number? b) (seconds->utc-time b) b)))
    (<= (date-value a) (date-value b))))

(define (date=? a b)
  (let ((a (if (number? a) (seconds->utc-time a) a))
        (b (if (number? b) (seconds->utc-time b) b)))
    (= (date-value a) (date-value b))))

(define (time<=? a b)
  (<= (if (number? a) a (utc-time->seconds a))
      (if (number? b) b (utc-time->seconds b))))

(define (time>=? a b)
  (>= (if (number? a) a (utc-time->seconds a))
      (if (number? b) b (utc-time->seconds b))))

(define (time=? a b)
  (= (if (number? a) a (utc-time->seconds a))
     (if (number? b) b (utc-time->seconds b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *epoch-year* 1970)

(define *start-of-month-vector*
  '#(0 31 59 90 120 151 181 212 243 273 304 334))

(define (start-of-month year month)
  (+ (vector-ref *start-of-month-vector* (- month 1))
     (if (and (> month 2) (leap-year? year)) 1 0)))

(define (leap-year? year)
  (and (zero? (modulo year 4))
       (or (not (zero? (modulo year 100)))
           (zero? (modulo year 400)))))

(define (day-of-year year month day)
  (+ (start-of-month year month) day))

(define day-of-week
  (let ((offsets '#(0 3 2 5 0 3 5 1 4 6 2 4)))
    (lambda (y m d)
      (let ((y (if (< m 3) (- y 1) y)))
        (remainder
         (+ y (quotient y 4) (- (quotient y 100)) (quotient y 400)
            (vector-ref offsets (- m 1)) d)
         7)))))

(define (days-since-epoch year month day)
  (let ((y (- year *epoch-year*)))
    (+ (* y 365)                               ;; base days
       (quotient (- y 1) 4)                    ;; leap years
       (- (quotient (- y 1) 100))              ;;   - mod 100 non-leap years
       (quotient (+ y 299) 400)                ;;   + mod 400 leap years
       (- (day-of-year year month day) 1)      ;; days in year
       )))

(define (epoch-seconds->utc-time-vector secs)
  (let* ((days (inexact->exact (quotient secs (* 24 60 60))))
         (year (+ *epoch-year* (quotient days 365)))
         (days-at-start-of-year (days-since-epoch year 1 1))
         (year (if (< days days-at-start-of-year) (- year 1) year))
         (day-of-year (- days (days-since-epoch year 1 1)))
         (month-estimate (+ 1 (quotient day-of-year 30)))
         (month (if (> (start-of-month year month-estimate) day-of-year)
                    (- month-estimate 1)
                    month-estimate))
         (day (+ 1 (- day-of-year (start-of-month year month))))
         (remaining-secs (inexact->exact (remainder secs (* 24 60 60))))
         (hours (quotient remaining-secs (* 60 60)))
         (minutes (quotient (- remaining-secs (* hours 60 60)) 60))
         (seconds (remainder remaining-secs 60)))
    ;; The epoch when counting seconds is Jan 1 1970, but the tm
    ;; struct stores the year as the distance from 1900.  Yay.
    (vector seconds minutes hours day (- month 1) (- year 1900)
            (day-of-week year month day) day-of-year #f 0)))

(define (epoch-seconds->time-vector secs tz-offset)
  (let ((res (epoch-seconds->utc-time-vector (- secs tz-offset))))
    (vector-set! res 9 tz-offset)
    res))

)
