;; hato-db.scm -- simple string->object database
;;
;; Copyright (c) 2005-2008 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(use gdbm lolevel)

(cond-expand
 ((and chicken compiling)
  (declare
   (export db? db-file? open-db close-db db-ref db-set! db-delete!)))
 (else))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primarily for white-lists and message-id databases.

;; Uses GDBM - we could default to a file-based solution when GDBM
;; is absent, but then have to detect and understand both types at
;; run-time in case they later install GDBM.  Need to finish my own
;; extensible hash-tables.

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
  (apply gdbm-open file o))
(define (close-db db)
  (gdbm-close db))
(define (db-ref db key . o)
  (let ((s (gdbm-fetch db key)))
    (if s
      (call-with-input-string s read)
      (and (pair? o) (car o)))))
(define (db-set! db key val)
  (gdbm-store db key (with-output-to-string (cut write val))))
(define (db-delete! db key)
  (gdbm-delete db key))

