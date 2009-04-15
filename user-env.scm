;;;; user-env.scm -- safe user environments
;;
;; Copyright (c) 2005 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; This is an extension of sandbox that lets you create
;; safe-environments which re-enable standard I/O procedures but limited
;; to files which the user would normally have access to.  It's assumed
;; the controlling process is owned by a user with access to all files
;; in question, namely root.

;; This lets us run code as separate users without spawning a new
;; process, allowing us to build high-scale yet fully customizable
;; servers.

;; In the future this may be extended with alternate 'virtual' security
;; models and/or filesystems built on top of the underlying OS.

;; Procedure: make-user-environment user [group-id]
;;   Returns a new user-environment inhering the bindings of
;;   base-user-environment, with user-id set to that for USER (a
;;   username string or integer id) and group GROUP-ID (defaulting to
;;   the primary group-id of USER).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond-expand
 ((and compiling (not static))
  (declare
   (export make-user-environment touch-file)))
 (else))

(require-extension sandbox posix)

(define (file-user-readable? file user-id group-id)
  (let* ((stats (file-stat file))
         (mode (vector-ref stats 1)))
    (or (and (eq? user-id (vector-ref stats 3))
             (not (zero? (bitwise-and mode perm/irusr))))
        (and (eq? group-id (vector-ref stats 4))
             (not (zero? (bitwise-and mode perm/irgrp))))
        (not (zero? (bitwise-and mode perm/iroth))))))

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
         (file-user-writeable? (pathname-directory file) user-id group-id)
         'new))))

(define (touch-file file uid gid . o)
  (call-with-output-file file (lambda (out) #t))
  (if (memv (current-user-id) (list 0 uid))
      (change-file-owner file uid gid))
  (if (pair? o)
      (change-file-mode file (car o))))

(define-macro (safe-environment-extend! env . vars)
  (let ((e (gensym)))
    `(let ((,e ,env))
       ,@(map (lambda (v)
                (let ((name (if (pair? v) (car v) v))
                      (val (if (pair? v) (cadr v) v)))
                  `(safe-environment-set! ,e ',name ,val)))
              vars))))

(define (transform-let-optionals* form)
  (let ((tmp (gensym))
        (ls (car form))
        (params (cadr form))
        (body (cddr form)))
    (append
     `(let ((,tmp ,ls)))
     (let lp ((p params))
       (cond
         ((pair? p)
          (let ((var (caar p)) (default (cadar p)))
            `((let* ((,var (if (pair? ,tmp) (car ,tmp) ,default))
                     (,tmp (if (pair? ,tmp) (cdr ,tmp) '())))
                ,@(lp (cdr p))))))
         ((symbol? p)
          `((let ((,p ,tmp)) ,@body)))
         (else
          body))))))

(define (transform-and-let* form)
  (let ((params (car form))
        (body (cdr form)))
    (car
     (let lp ((ls params))
       (if (null? ls)
         body
         (let ((param (car ls)))
           (if (pair? (car param))
             `((and ,(car param) ,@(lp (cdr ls))))
             `((let (,param)
                 (and ,(car param) ,@(lp (cdr ls))))))))))))

(define *base-user-environment*
  (let ((e (make-safe-environment parent: default-safe-environment)))
    ;; enable safe macros
    (safe-environment-macro-set! e 'let-optionals* transform-let-optionals*)
    (safe-environment-macro-set! e 'and-let* transform-and-let*)
    ;; enable safe procedures
    (safe-environment-extend!
     e
     file-user-readable? file-user-writeable? input-port? output-port?
     close-input-port close-output-port with-input-from-string
     with-output-to-string make-parameter error
     with-input-from-port with-output-to-port
     current-input-port current-output-port flush-output
     read read-char read-line write write-char write-line
     display print printf fprintf sprintf newline
     peek-char char-ready? eof-object?)
    e))

(define (base-user-environment)
  *base-user-environment*)

(define (make-user-environment
         user
         #!key
         (parent (base-user-environment))
         (user-info (delay (user-information user)))
         (user-name (cond ((string? user) user)
                          (else (car (force user-info)))))
         (user-id (cond ((number? user) user)
                        ((zero? (current-user-id)) (caddr (force user-info)))
                        (else (current-user-id))))
         (group-id (cond ((zero? (current-user-id)) (cadddr (force user-info)))
                         (else (current-group-id)))))
  (let ((e (make-safe-environment parent: (base-user-environment))))
    ;; temporarily pass underlying I/O procedures
    (safe-environment-set! e '%open-input-file open-input-file)
    (safe-environment-set! e '%open-output-file open-output-file)
    (safe-environment-set! e '%touch-file touch-file)
    (safe-eval
     `(begin
        (define (current-user-name) ,user-name)
        (define (current-user-id) ,user-id)
        (define (current-group-id) ,group-id)
        (define (file-readable? file)
          (file-user-readable? file (current-user-id) (current-group-id)))
        (define (file-writeable? file)
          (file-user-writeable? file (current-user-id) (current-group-id)))
        (define open-input-file
          (let ((%open-input-file %open-input-file))
            (lambda (file)
              (if (file-readable? file)
                  (%open-input-file file)
                  (error "file not readable: " file)))))
        (define open-output-file
          (let ((%open-output-file %open-output-file))
            (lambda (file)
              (let ((ok? (file-writeable? file)))
                (if (eq? ok? 'new) ; create an empty file owned by the user
                    (%touch-file file (current-user-id) (current-group-id)))
                (if ok?
                    (%open-output-file file)
                    (error "file not writeable: " file))))))
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
                 (res (with-input-form-port port thunk)))
            (close-input-port port)
            res))
        (define (with-output-to-file file thunk)
          (let* ((port (open-output-file file))
                 (res (with-output-to-port port thunk)))
            (close-output-port port)
            res))
        )
     environment: e)
    ;; remove core I/O procedures
    (for-each
     (cut safe-environment-remove! e <>)
     '(%open-input-file %open-output-file %touch-file))
    e))

