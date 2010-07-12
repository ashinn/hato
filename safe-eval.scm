;;;; safe-eval.scm -- sandboxed eval using chicken's module system
;;
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-library safe-scheme safe-io)

(module safe-eval
  (safe-eval safe-wrap-expression
   safe-apply-as-user safe-eval-as-user safe-wrap-user-expression
   safe-standard-imports safe-extended-imports)

(import scheme chicken posix
        (only safe-io current-safe-user-id current-safe-group-id))

(define (safe-wrap-expression expr imports)
  `(module ,(gensym 'safe) ()
     (import safe-scheme ,@imports)
     (let-syntax ((import
                   (er-macro-transformer
                    (lambda (e r c)
                      (error "can't import in controlled environment"))))
                  (define-syntax
                    (er-macro-transformer
                     (lambda (e r c)
                       (error "can't define-syntax in controlled environment")
                       ))))
       ,expr)))

(define (safe-eval expr . o)
  (eval (safe-wrap-expression expr (if (pair? o) (car o) '()))))

(define (safe-wrap-user-expression expr imports)
  `(module ,(gensym 'safe) ()
     (import safe-scheme
             (except safe-io current-safe-user-id current-safe-group-id)
             ,@imports)
     (let-syntax ((import
                   (er-macro-transformer
                    (lambda (e r c)
                      (error "can't import in controlled environment"))))
                  (define-syntax
                    (er-macro-transformer
                     (lambda (e r c)
                       (error "can't define-syntax in controlled environment")
                       ))))
       (lambda () ,expr))))

(define (safe-apply-as-user thunk user . o)
  (let* ((info (and (string? user) (user-information user)))
         (user-id (if info (caddr info) user))
         (group-id (or (and (pair? o) (car o))
                       (begin
                         (if (not info)
                             (set! info (user-information user-id)))
                         (cadddr info)))))
    (parameterize ((current-safe-user-id user-id)
                   (current-safe-group-id group-id))
      (thunk))))

(define (safe-eval-as-user expr imports . o)
  (apply safe-apply-as-user (eval (safe-wrap-user-expression expr imports)) o))

(define safe-standard-imports
  '((only chicken
          add1            fxshl
          bit-set?        fxshr
          bitwise-and     fxxor
          bitwise-ior     gensym
          bitwise-not     get-keyword
          bitwise-xor     get-output-string
          blob->string    keyword->string
          blob-size       keyword-style
          blob?           keyword?
          blob=?          make-blob
          call/cc         make-composite-condition
          char-name       make-parameter
          error           make-property-condition
          fixnum?         open-input-string
          flonum?         open-output-string
          flush-output    port-name
          fp-             port-position
          fp*             port?
          fp/             print
          fp+             print-error-message
          fp<             print*
          fp<=            promise?
          fp=             reverse-list->string
          fp>             string->blob
          fp>=            string->keyword
          fpmax           string->uninterned-symbol
          fpmin           strip-syntax
          fpneg           sub1
          fx-             syntax-error
          fx*             vector-resize
          fx/             void
          fx+             with-exception-handler
          fx<             arithmetic-shift
          fx<=            condition-predicate
          fx=             condition-property-accessor
          fx>             condition?
          fx>=            continuation-capture
          fxand           continuation-graft
          fxior           continuation-return
          fxmax           continuation?
          fxmin           current-exception-handler
          fxmod           current-milliseconds
          fxneg           current-seconds
          fxnot
          )
    (only extras
          (format fprintf pp pretty-print printf random random-seed
           randomize read-byte read-line read-lines read-string read-string!
           read-token sprintf write-byte write-line write-string))
    (only ports
          (call-with-input-string call-with-output-string make-input-port
           make-output-port port-for-each port-map port-fold
           make-broadcast-port make-concatenated-port with-error-output-to-port
           with-input-from-port with-input-from-string with-output-to-port
           with-output-to-string with-error-output-to-port))
    (only data-structures
          (->string alist-ref alist-update! always? any? atom? binary-search
           butlast chop complement compose compress conc conjoin constantly
           disjoin each flatten flip identity intersperse join left-section
           list->queue list-of? make-queue merge merge! never? none? noop o
           project queue->list queue-add! queue-empty? queue-first queue-last
           queue-push-back! queue-push-back-list! queue-remove! queue? rassoc
           right-section shuffle sort sort! sorted? string-chomp string-chop
           string-compare3 string-compare3-ci string-intersperse string-split
           string-translate string-translate* substring-ci=? substring-index
           substring-index-ci substring=? tail?))))

(define safe-extended-imports
  (append
   safe-standard-imports
   '(srfi-1 srfi-4 srfi-13 srfi-14)))

)

