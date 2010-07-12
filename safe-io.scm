;;;; safe-io.scm -- restricted user I/O for use with safe-eval
;;
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-library posix)

(module safe-io
    (current-safe-user-id current-safe-group-id
     file-user-readable? file-user-writeable? touch-file
     file-readable? file-writeable?
     open-input-file open-output-file
     call-with-input-file call-with-output-file
     with-input-from-file with-output-to-file
     ;; load delete-file rename-file
     )

(import (except (rename scheme                        
                        (open-input-file %open-input-file)
                        (open-output-file %open-output-file))
                with-input-from-file with-output-to-file
                call-with-input-file call-with-output-file)
        chicken extras posix files ports)

(define current-safe-user-id (make-parameter (current-user-id)))
(define current-safe-group-id (make-parameter (current-group-id)))

(define (file-user-readable? file user-id group-id)
  (condition-case
      (let* ((stats (file-stat file))
             (mode (vector-ref stats 1)))
        (or (and (eq? user-id (vector-ref stats 3))
                 (not (zero? (bitwise-and mode perm/irusr))))
            (and (eq? group-id (vector-ref stats 4))
                 (not (zero? (bitwise-and mode perm/irgrp))))
            (not (zero? (bitwise-and mode perm/iroth)))))
    (exn () #f)))

(define (file-user-writeable? file user-id group-id)
  (condition-case
      (let* ((stats (file-stat file))
             (mode (vector-ref stats 1)))
        (or (and (eq? user-id (vector-ref stats 3))
                 (not (zero? (bitwise-and mode perm/iwusr))))
            (and (eq? group-id (vector-ref stats 4))
                 (not (zero? (bitwise-and mode perm/iwgrp))))
            (not (zero? (bitwise-and mode perm/iwoth)))))
    (exn ()
         (and (not (file-exists? file))
              (not (equal? (pathname-directory file) file))
              (condition-case
                  (let* ((stats (file-stat (or (pathname-directory file) ".")))
                         (mode (vector-ref stats 1)))
                    (or (and (eq? user-id (vector-ref stats 3))
                             (not (zero? (bitwise-and mode perm/iwusr))))
                        (and (eq? group-id (vector-ref stats 4))
                             (not (zero? (bitwise-and mode perm/iwgrp))))
                        (not (zero? (bitwise-and mode perm/iwoth)))))
                (exn () #f))
              'new))))

(define (touch-file file uid gid . o)
  (let ((out (%open-output-file file)))
    (close-output-port out))
  (if (memv (current-user-id) (list 0 uid))
      (change-file-owner file uid gid))
  (if (pair? o)
      (change-file-mode file (car o))))

(define (file-readable? file)
  (file-user-readable? file (current-safe-user-id) (current-safe-group-id)))

(define (file-writeable? file)
  (file-user-writeable? file (current-safe-user-id) (current-safe-group-id)))

(define open-input-file
  (lambda (file)
    (if (file-readable? file)
        (%open-input-file file)
        (error "file not readable: " file))))

(define open-output-file
  (lambda (file)
    (let ((ok? (file-writeable? file)))
      (if (eq? ok? 'new)      ; create an empty file owned by the user
          (touch-file file (current-safe-user-id) (current-safe-group-id)))
      (if ok?
          (%open-output-file file)
          (error "file not writeable: " file)))))

(define (call-with-input-file file proc)
  (let* ((p (open-input-file file))
         (res (proc p)))
    (close-input-port p)
    res))

(define (call-with-output-file file proc)
  (let* ((p (open-output-file file))
         (res (proc p)))
    (close-output-port p)
    res))

(define (with-input-from-file file thunk)
  (let* ((port (open-input-file file))
         (res (with-input-from-port port thunk)))
    (close-input-port port)
    res))

(define (with-output-to-file file thunk)
  (let* ((port (open-output-file file))
         (res (with-output-to-port port thunk)))
    (close-output-port port)
    res))

 )
