;;;; count-collisions.scm -- count # of hash collisions in a corpus
;;
;; Copyright (c) 2005 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; This is a simple tool to compute the number of hash collisions in a
;; corpus for a given key size and token chain-length.  You can pass the
;; -md5 option to see the results with md5 hashing instead of the
;; default hashing - in all my test cases this is *extremely* slow and
;; offers almost exactly the same results as our default hash based on
;; Bernstein's algorithm (we use salt so actual results will vary on
;; each run).

;; WARNING: this requires a huge amount of RAM for large corpuses with
;; large chain-lengths.

;; TODO: add options for mime/html decoding + string normalization.

(include "let-args.scm")

(use md5 srfi-1 format regex posix utils hato-prob)

(define (mail-files file)
  (cond
    ((directory? file)
     (append-map mail-files (map (cut string-append file "/" <>)
                                 (cddr (directory file)))))
    ((file-exists? file)
     (list file))
    (else
     (fprintf (current-error-port) "ignoring non-existant file: ~S\n" file)
     (list '()))))

(define (count-duplicates total-features vec verbose?)
  (let lp ((i (- (vector-length vec) 1)) (features 0) (count 0) (sum 0))
    (if (negative? i)
      (begin
        (printf "~S features (~S distinct)\n" total-features features)
        (printf "~S collisions in ~S of ~S total buckets\n"
                sum count (vector-length vec)))
      (let* ((ls (vector-ref vec i))
             (len (length ls)))
        (if (>= len 2)
          (begin
            (if verbose?
              (printf "~S: ~S\n" i ls))
            (lp (fx- i 1) (fx+ features len) (fx+ count 1) (fx+ sum len)))
          (lp (fx- i 1) (fx+ features len) count sum))))))

(define (insert-sorted-unique! ls elt . opt)
  (let ((sort<= (if (pair? opt) (car opt) <=)))
    (if (null? ls)
      (list elt)
      (let loop ((l ls))
        (let ((first (car l)))
          (if (sort<= elt first)
            (cond
              ((sort<= first elt)
               ls) ; equal
              (else
                (set-cdr! l (cons first (cdr l)))
                (set-car! l elt)
                ls))
            (let ((rest (cdr l)))
              (cond
                ((null? rest)
                 (set-cdr! l (list elt))
                 ls)
                (else
                 (loop rest))))))))))

(define (chain-token hash-key vec len words tok)
  (let lp ((i (fx- len 1)))
    (if (zero? i)
      (vector-set! words 0 tok)
      (let ((prev (fx- i 1)))
        (vector-set! words i (string-append (vector-ref words prev) " " tok))
        (lp prev))))
  (let lp ((i (fx- len 1)))
    (when (fx>= i 0)
      (let* ((word (vector-ref words i))
             (bucket (hash-key word))
             (old (vector-ref vec bucket)))
        (vector-set! vec bucket (insert-sorted-unique! old word string<=?))
        (lp (fx- i 1))))))

(define (main args)
  (let-args args
      ((key-size "k|key-size=i" 2)
       (chain-length "c|chain-length=i" 1)
       (verbose? "V|verbose" #f)
       (md5? "5|md5" #f)
       . files)
    (let* ((num-keys (expt 2 (* 8 key-size)))
           (mask (inexact->exact (- num-keys 1)))
           (vec (make-vector num-keys '()))
           (words (make-vector chain-length ""))
           (salt 23)
           (db (make-i3db #f #f #f 0 0 mask 1 salt #f #f))
           (hash-key (if md5?
                       (lambda (s)
                         (string->number (substring (md5:digest s) 0 (fx* 2 key-size)) 16))
                       (lambda (s)
                         (i3db-hash db s 0 (string-length s) (i3db-salt db)))))
           (features 0))
      (if verbose?
        (printf "key-size: ~S chain-length: ~S files: ~S\n"
                key-size chain-length files))
      (for-each
       (lambda (file)
         (with-input-from-file file
           (lambda ()
             (let line-loop ()
               (let ((line (read-line)))
                 (unless (eof-object? line)
                   (for-each
                    (lambda (tok)
                      (chain-token hash-key vec chain-length words tok)
                      (set! features (fx+ features chain-length)))
                    (token-fold line '()
                                (lambda (s i j a)
                                  (cons (substring s i j) a))))
                   (line-loop)))))))
       (append-map mail-files files))
      (count-duplicates features vec verbose?))))

(main (command-line-arguments))

