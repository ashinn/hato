;; hato-db.scm -- simple string->object database
;;
;; Copyright (c) 2005-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(require-library tokyocabinet)

(module hato-db
  (db? db-file? open-db close-db db-ref db-set! db-delete!)

(import scheme chicken tokyocabinet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primarily for white-lists and message-id databases.

(define (db-file? file)
  (define (ascii? c)
    (or (<= #x20 c #x7F)
        (memv c '(#x09 #x0A #x0D))))
  (and (file-exists? file)
       (call-with-input-file file
         (lambda (in) ; XXXX check the magic
           (or (not (ascii? (read-byte in)))
               (not (ascii? (read-byte in)))
               (not (ascii? (read-byte in)))
               (not (ascii? (read-byte in))))))))

(define db? pointer?)
(define (open-db file . o)
  (apply tc-hdb-open file o))
(define (close-db db)
  (tc-hdb-close db))
(define (db-ref db key . o)
  (let ((s (tc-hdb-get db key)))
    (if s
        (call-with-input-string s read)
        (and (pair? o) (car o)))))
(define (db-set! db key val)
  (tc-hdb-put! db key (with-output-to-string (cut write val))))
(define (db-delete! db key)
  (tc-hdb-delete! db key))

)
