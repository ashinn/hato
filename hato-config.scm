;; hato-config.scm -- loading and referencing config files
;;
;; Copyright (c) 2005-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-library srfi-1 srfi-13 srfi-69 matchable hato-db)

(module hato-config
 (assq-ref read-from-file alist? conf?
  conf-load conf-load-in-path conf-load-cascaded
  conf-get conf-get-list conf-get-alist
  conf-multi conf-extend conf-verify
  conf-load-table)

(import scheme chicken data-structures srfi-1 srfi-13 srfi-69 extras utils files posix ports hato-db)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config lists (cascaded lists of alists)

(define (assq-ref ls key . o)
  (let ((cell (assq key ls)))
    (if cell (cdr cell) (and (pair? o) (car o)))))

(define (read-from-file file . opt)
  (condition-case (call-with-input-file file read)
    (exn () (and (pair? opt) (car opt)))))

(define (alist? x)
  (and (list? x) (every pair? x)))

(define (conf? x)
  (and (list? x) (every alist? x)))

(define (conf-load-cascaded cfg-path file)
  (let ((res (conf-load-in-path
              cfg-path
              (if (symbol? file) (symbol->string file) file))))
    (append res (map (lambda (x) (conf-load-cascaded cfg-path x))
                     (conf-get-list (list res) 'inherits)))))

(define (conf-load-in-path cfg-path file)
  (filter
   pair?
   (append-map
    (lambda (dir)
      (conf-load
       (if (eqv? #\/ (string-ref file 0)) file (make-pathname dir file))))
    cfg-path)))

(define (conf-load file . o)
  (cons
   (parameterize ((case-sensitive #f))
     (read-from-file file '()))
   (if (pair? o) (car o) '())))

(define (conf-get-cell cfg key)
  (let search ((ls cfg))
    (and (pair? ls)
         (or (assq key (car ls))
             (search (cdr ls))))))

(define (conf-get cfg key . opt)
  (let ((cell (conf-get-cell cfg key)))
    (if (not cell)
        (and (pair? opt) (car opt))
        (if (and (pair? (cdr cell)) (null? (cddr cell)))
            (cadr cell)
            (cdr cell)))))

(define (conf-get-list cfg key . opt)
  (let ((res (conf-get cfg key)))
    (if res
        (if (or (pair? res) (null? res)) res (list res))
        (if (pair? opt) (car opt) '()))))

(define (conf-get-alist cfg key . opt)
  (let ((res (conf-get cfg key)))
    (if res
        (if (null? res)
            res
            (if (and (pair? res) (pair? (car res)))
                res
                (list res)))
        (if (pair? opt) (car opt) '()))))

(define (conf-multi cfg key)
  (append-map (cut assq-ref <> key '()) cfg))

(define (conf-extend alist cfg)
  (if (pair? alist)
      (cons alist cfg)
      cfg))

;; convert a config setting to a table for lookups.  can be either a
;; list, which is converted directly to a hash-table, or a file, which
;; is either opened as a db file (thus reflecting run-times changes) or
;; if a text file is loaded assuming one line per entry
(define (conf-load-table x)
  (cond ((or (pair? x) (null? x))
         (alist->hash-table (map (lambda (x) (cons x #t)) x) string=?))
        ((string? x)
         (and (file-exists? x)
              (if (db-file? x)
                  (open-db x)
                  (conf-load-table
                   (remove
                    string-null?
                    (with-input-from-file x
                      (port-map string-trim-both read-line)))))))
        (else
         #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conf verification tests

(define (conf-verify cfg spec . o)
  (let ((warn (if (pair? o) (car o) warning)))
    (for-each
     (lambda (alist)
       (for-each
        (lambda (cell) (conf-verify-one cell spec warn))
        alist)) 
     cfg)))

(define (conf-verify-one cell spec warn)
  (cond
   ((not (pair? cell))
    (warn (sprintf "bad config entry: ~S" cell)))
   ((not (symbol? (car cell)))
    (warn (sprintf "non-symbol config entry name: ~S" (car cell))))
   (else
    (let ((def (assq (car cell) spec)))
      (cond
       ((not def)
        (warn (sprintf "unknown config entry: ~S" (car cell))))
       ((null? (cdr def)))
       (else (conf-verify-match cell (cadr def) warn)))))))

(define (conf-verify-symbol->predicate sym)
  (case sym
    ((integer) integer?)
    ((number) number?)
    ((list) list?)
    ((alist) alist?)
    ((boolean) boolean?)
    ((char) char?)
    ((string) string?)
    ((symbol) symbol?)
    ((pair) pair?)
    ((filename dirname) string?)
    (else (error "no known conf predicate for" sym))))

;; non-short-circuit versions

(define (and* . args)
  (every identity args))

(define (every* pred ls)
  (apply and* (map pred ls)))

(define (conf-verify-match cell def warn)
  (define (cell-value)
    (if (and (pair? (cdr cell)) (null? (cddr cell))) (cadr cell) (cdr cell)))
  (define (cell-list)
    (if (and (pair? (cdr cell)) (null? (cddr cell)) (not (pair? (cadr cell))))
        (list (cadr cell))
        (cdr cell)))
  (cond
   ((procedure? def)
    (or (def (cell-value))
        (begin
          (warn (sprintf "bad conf value for ~S: ~S" (car cell) (cell-value)))
          #f)))
   ((symbol? def)
    (case def
      ((existing-filename)
       (cond
        ((not (string? (cell-value)))
         (warn
          (sprintf "bad conf value for ~S: expected a filename but got ~S"
                   (car cell) (cell-value)))
         #f)
        ((not (file-exists? (cell-value)))
         (warn
          (sprintf "conf setting ~S references a non-existent file: ~S"
                   (car cell) (cell-value)))
         #f)
        (else
         #t)))
      ((existing-dirname)
       (cond
        ((not (string? (cell-value)))
         (warn
          (sprintf "bad conf value for ~S: expected a dirname but got ~S"
                   (car cell) (cell-value)))
         #f)
        ((not (directory? (cell-value)))
         (cond
          ((file-exists? (cell-value))
           (warn
            (sprintf "conf setting ~S expected a directory but found a file: ~S"
                     (car cell) (cell-value)))
           #f)
          (else
           (warn
            (sprintf "conf setting ~S references a non-existent directory: ~S"
                     (car cell) (cell-value)))
           #f)))
        (else
         #t)))
      ((integer number char string symbol filename dirname boolean pair)
       (or ((conf-verify-symbol->predicate def) (cell-value))
           (begin
             (warn (sprintf "bad conf value for ~S: expected ~S but got ~S"
                            (car cell) def (cell-value)))
             #f)))
      ((list alist)
       (or ((conf-verify-symbol->predicate def) (cell-list))
           (begin
             (warn (sprintf "bad conf value for ~S: expected ~S but got ~S"
                            (car cell) def (cell-list)))
             #f)))
      (else
       (warn (sprintf "bad conf spec list: ~S" def))
       #f)))
   ((pair? def)
    (case (car def)
      ((cons)
       (and*
        (conf-verify-match
         (cons `(car ,(car cell)) (car (cell-list))) (cadr def) warn)
        (conf-verify-match
         (cons `(car ,(car cell)) (cdr (cell-list))) (caddr def) warn)))
      ((list)
       (and (list? (cell-list))
            (every* (lambda (x)
                      (conf-verify-match
                       (cons `(list ,(car cell)) x) (cadr def) warn))
                    (cell-list))))
      ((alist)
       ;;(every*
       ;; (lambda (cell) (conf-verify-one cell (cdr def) warn))
       ;; (cell-list))
       (alist? (cell-list))
       )
      ((or)
       (or (any (lambda (x) (conf-verify-match cell x identity))
                (cdr def))
           (begin
             (warn (sprintf "bad spec value for ~S: expected ~S but got ~S"
                            (car cell) def (cell-value)))
             #f)))
      ((member)
       (or (member (cell-value) (cdr def))
           (begin
             (warn (sprintf "bad spec value ~S for ~S, expected one of ~S"
                            (cell-value) (car cell) (cdr def)))
             #f)))
      ((quote)
       (or (equal? (cadr def) (cell-value))
           (begin
             (warn (sprintf "bad conf value for ~S: expected '~S but got ~S"
                            (car cell) (cadr def) (cell-value)))
             #f)))
      (else
       (warn (sprintf "bad conf list spec name: ~S" (car def)))
       #f)))
   (else
    (or (equal? def (cell-value))
        (begin
          (warn (sprintf "bad conf value for ~S: expected ~S but got ~S"
                         (car cell) def (cell-value)))
          #f)))))

 )
