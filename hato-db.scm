;; hato-db.scm -- simple string->object database
;;
;; Copyright (c) 2005-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(require-library tokyocabinet)

(module hato-db
  (db? db-file? open-db close-db db-ref db-set! db-delete!)

(import scheme chicken extras ports lolevel tokyocabinet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primarily for white-lists and message-id databases.

(define (db-file? file)
  (and (file-exists? file)
       (equal? "ToKyO CaBiNeT" (call-with-input-file file read-line))))

(define db? pointer?)
(define (open-db file . o)
  (let* ((read-only? (and (pair? o) (car o)))
         (flags
          (if read-only?
              (fx+ TC_HDBOREADER TC_HDBOCREAT)
              (fx+ TC_HDBOWRITER (fx+ TC_HDBOREADER TC_HDBOCREAT)))))
    (tc-hdb-open file #:flags flags #:tune-opts TC_HDBTDEFLATE)))
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
