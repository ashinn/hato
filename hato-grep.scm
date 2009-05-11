#!/usr/local/bin/csi -script
;;;; hato-grep.scm -- grep mail for keywords
;;
;; Copyright (c) 2008-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use let-args numbers posix extras utils regex srfi-1 srfi-13
     hato-archive hato-uri hato-mime hato-date)

(define *program-name* "hato-grep")
(define-syntax read-version
  (er-macro-transformer (lambda _ (call-with-input-file "VERSION" read-line))))
(define *program-version* (read-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-help . o)
  (display
"usage: hato-grep pattern mailboxes ...
")
  (exit))

(define (show-version . o)
  (display *program-name*)
  (display " ")
  (display *program-version*)
  (newline)
  (exit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (grep-message rx path text after)
  (condition-case
    (call-with-input-string text
      (lambda (in)
        (mime-message-fold
         in (lambda (ph h b a) (grep-body rx path b after) '()) '())))
    (exn ()
         (print-error-message
          exn (current-error-port)
          (sprintf "hato-grep: error processing ~S" (or path "-"))))))

(define (grep-body rx path text after)
  (let lp ((i 1)
           (n 0)
           (ls (string-split text "\n" #t)))
    (if (pair? ls)
        (let ((line (car ls)))
          (cond
           ((string-search rx line)
            (printf "~A:~S: ~A\n" path i line)
            (lp (+ i 1) after (cdr ls)))
           ((positive? n)
            (printf "~A:~S: ~A\n" path i line)
            (lp (+ i 1) (- n 1) (cdr ls)))
           (else
            (lp (+ i 1) n (cdr ls))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main args)
  (let-args args
      ((help "help|h" => show-help)
       (version "version|V" => show-version)
       (after "after|A=n" 0)
       (else (opt rest cont) (error "invalid option" opt))
       . rest)
    (let ((rx (regexp (car rest) #t))
          (mboxes (cdr rest)))
      (for-each
       (lambda (mbox)
         (mail-archive-fold mbox (lambda (f t a) (grep-message rx mbox t after)) #f))
       mboxes))))

(main (command-line-arguments))
