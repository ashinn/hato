#!/usr/local/bin/csi -script
;;;; hato-cat.scm -- decode messages
;;
;; Copyright (c) 2008 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(include "let-args.scm")
(use numbers posix gdbm extras utils regex srfi-1 srfi-13
     hato-archive hato-uri hato-mime hato-date)

(define *program-name* "hato-cat")
(define-macro (read-version)
  (call-with-input-file "VERSION" read-line))
(define *program-version* (read-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-help . o)
  (display
"usage: hato-cat [options] mailboxes ...
")
  (exit))

(define (show-version . o)
  (display *program-name*)
  (display " ")
  (display *program-version*)
  (newline)
  (exit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-message text extract)
  (call-with-input-string text
    (lambda (in)
      (let ((headers (mime-headers->list in)))
        (mime-message-fold
         in
         headers
         (lambda (h b a)
           (for-each
            (lambda (e)
              (cond ((mime-ref h e)
                     => (lambda (v) (print (string-titlecase e) ": " v)))))
            extract)
           (print b))
         #f)
        (newline)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main args)
  (let-args args
      ((help "help|h" => show-help)
       (version "version|V" => show-version)
       (extract "extract|x=s")
       (else (opt rest cont) (error "invalid option" opt))
       . mboxes)
    (for-each
     (lambda (mbox)
       (mail-archive-fold
        mbox
        (lambda (f t a)
          (print-message t (string-split (or extract "") ",")))
        #f))
     mboxes)))

(main (command-line-arguments))
