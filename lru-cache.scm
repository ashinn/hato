;;; lru-cache.scm -- simple least-recently-used cache
;;
;; Copyright (c) 2005-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; Procedure: make-lru #!key equal size-limit compute-size
;;
;; Creates a new empty LRU object.  EQUAL is an equality function
;; defaulting to EQUAL?.  Currently it must be usable as an equality
;; function as in MAKE-HASH-TABLE, which means you can't use STRING-CI=?
;; or other predicates for which the default hash function won't work.
;; SIZE-LIMIT is an integer designating the maximum size of the cache.
;; COMPUTE-SIZE is a procedure of two arguments, the key and value to be
;; stored, and defaults to a constant 1 per entry.  You may find
;;
;;   (lambda (k v) (+ (object-size k) (object-size v)))
;;
;; using OBJECT-SIZE from unit lolevel to be a useful COMPUTE-SIZE.

;; Procedure: lru-ref lru key [compute]
;;
;; Looks up KEY in the cache LRU.  If not found returns #f, unless
;; COMPUTE is given in which case COMPUTE is applied to KEY to determine
;; the return value.  This does not update the cache.

;; Procedure: lru-ref! lru key [compute]
;;
;; Identical to lru-ref except that it updates the cache on a miss.

;; Procedure: lru-set! lru key value
;;
;; Directly set a value in the cache.

(module lru-cache
  (make-lru-cache lru-cache? lru-ref lru-ref! lru-set!)

(import scheme chicken srfi-69)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple transparent queues

(define-inline (make-q) (cons '() '())) ; (first . last)
(define-inline (q-empty? q) (null? (car q)))
(define-inline (q-first q) (caar q))
(define-inline (q-last q) (cadr q))
(define-inline (q-last-pair q) (cdr q))

(define (enq! q x)
  (let ((cell (list x)))
    (if (q-empty? q)
      (set-car! q cell)
      (set-cdr! (cdr q) cell))
    (set-cdr! q cell)))

(define (deq! q)
  (let* ((cell (car q))
         (rest (cdr cell)))
    (set-car! q rest)
    (if (null? rest) (set-cdr! q '()))
    (car cell)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant hashtab-default-size 31)

(define-record-type %lru-cache
  (%make-lru-cache table queue size size-limit compute-size)
  lru-cache?
  (table lru-table)
  (queue lru-queue)
  (size lru-size set-lru-size!)
  (size-limit lru-size-limit)
  (compute-size lru-compute-size))

(define (make-lru-cache
         #!key
         (equal-pred equal?)
         (hash-func hash)
         (init-size hashtab-default-size)
         (compute-size (lambda (k v) 1))
         (size-limit 100))
  (let ((tab (make-hash-table equal-pred hash-func init-size)))
    (%make-lru-cache tab (make-q) 0 size-limit compute-size)))

(define-syntax lru-entry
  (syntax-rules () ((lru-entry args ...) (vector args ...))))
(define-inline (lru-entry-key e)   (vector-ref e 0))
(define-inline (lru-entry-value e) (vector-ref e 1))
(define-inline (lru-entry-size e)  (vector-ref e 2))
(define-inline (lru-entry-prev e)  (vector-ref e 3))
(define-inline (set-lru-entry-value! e v) (vector-set! e 1 v))
(define-inline (set-lru-entry-prev! e v)  (vector-set! e 3 v))

(define (lru-shift-to-last! lru e)
  (let* ((prev (lru-entry-prev e))
         (queue (lru-queue lru))
         (last-pair (q-last-pair queue)))
    (if (null? prev) ; first entry
      (begin
        (enq! queue (deq! queue))
        (set-lru-entry-prev! e last-pair)
        (set-lru-entry-prev! (caar queue) '()))
      (let ((cell (cdr prev)))
        (unless (eq? cell last-pair)
          (set-cdr! prev (cdr cell)) ; splice out
          (if (pair? (cdr cell))
            (set-lru-entry-prev! (cadr cell) prev))
          (enq! queue e)             ; reinsert at end
          (set-lru-entry-prev! e last-pair))))))

(define (lru-ref lru key . o)
  (let ((e (hash-table-ref/default (lru-table lru) key #f)))
    (if e
      (begin
        (lru-shift-to-last! lru e)
        (lru-entry-value e))
      (and (pair? o) ((car o) key)))))

(define (lru-ref! lru key . o)
  (lru-ref lru key
    (lambda (k) (and (pair? o) (let ((v ((car o) k))) (lru-add! lru k v) v)))))

(define (lru-shrink! lru)
  (let ((size-limit (lru-size-limit lru))
        (queue (lru-queue lru))
        (tab (lru-table lru)))
    (let lp ((size (lru-size lru)))
      (if (> size size-limit)
        (let ((x (deq! queue)))
          (let ((next (car queue)))
            (when (pair? next)
              (set-lru-entry-prev! (car next) '())))
          (hash-table-delete! tab (lru-entry-key x))
          (lp (- size (lru-entry-size x))))
        (set-lru-size! lru size)))))

(define (lru-add! lru key val)
  (let* ((size ((lru-compute-size lru) key val))
         (queue (lru-queue lru))
         (last-pair (q-last-pair queue))
         (e (lru-entry key val size last-pair)))
    (hash-table-set! (lru-table lru) key e)
    (enq! queue e)
    (set-lru-size! lru (+ size (lru-size lru)))
    (lru-shrink! lru)))

(define (lru-set! lru key val)
  (let ((e (hash-table-ref/default (lru-table lru) key #f)))
    (if e
      (begin
        (lru-shift-to-last! lru e)
        (set-lru-entry-value! e val)
        (set-lru-size! lru ((lru-compute-size lru) key val))
        (lru-shrink! lru))
      (lru-add! lru key val))))

; (define (lru-validate lru)
;   (let lp ((q (car (lru-queue lru))) (prev #f))
;     (when (pair? q)
;       (let* ((cur (car q))
;              (cur-prev-cell (lru-entry-prev cur))
;              (cur-prev (and (pair? cur-prev-cell) (car cur-prev-cell))))
;         (when (not (eq? prev cur-prev))
;           (fprintf (current-error-port) "~A's prev should be ~A but is ~A\n"
;                    (lru-entry-key cur)
;                    (and (vector? prev) (lru-entry-key prev))
;                    (and (vector? cur-prev) (lru-entry-key cur-prev))))
;         (lp (cdr q) cur)))))

)
