;;;; run-compare.scm -- run comparison of classifier with different settings
;;
;; Copyright (c) 2005 Alex Shinn.  All rights reserved.
;; BSD-style license: http://www.debian.org/misc/bsd.license

(include "let-args.scm")

(cond-expand
 (static
  (include "hato-prob.scm"))
 (else
  (require-extension hato-prob hato-i3db)
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

;; XXXX there's duplication of default values between this and
;; feature-fold, need to find an elegant way to eliminate that.
(define (main args)
  (let-args (cdr args)
      ((ham? "ham" #f)
       (spam? "spam" #f)
       (exit-code? "e|exit-code" #f)
       (insert-header? "i|insert-header" #f)
       (print-result? "p|print-result" #f)
       (literal? "literal" #f)
       (case-insensitive? "case-insensitive" #f)
       (deleet? "deleet" #f)
       (auto-learn? "a|auto-learn" #f)
       (verbose? "verbose" #f)
       (delete-database? "delete-database" #f)
       (refile? "refile" #f)
       (no-update-count? "no-update-count" #f)
       (mime? "mime" #f)
       (html? "html" #f)
       (prop-prob? "prop|proportional-probability" #f)
       (naive-bayes? "naive-bayes" #f)
       (robinson? "robinson" #f)
       (db-file "db-file=s" #f)
       (offset "offset=i" 1)
       (key-size "k|key-size=i" 2)
       (value-size "v|value-size=i" 2)
       (min-length "m|min-length=i" 1)
       (num-significant "n|num-significant=i" 0)
       (chain-length "c|chain|chain-length=i" 1)
       (threshold "t|threshold=f" 0.6)
       (weight-factor "w|weight-factor=i" 2)
       (epsilon "epsilon=f" 0.0)
       . files)
    (if delete-database?
      (let ((db-file (i3db-file-name key-size value-size)))
        (if (file-exists? db-file)
          (delete-file db-file)))
      (let ((a
             (feature-fold
              files
              ham?: ham?
              spam?: spam?
              insert-header?: insert-header?
              print-result?: print-result?
              literal?: literal?
              case-insensitive?: case-insensitive?
              deleet?: deleet?
              auto-learn?: auto-learn?
              verbose?: verbose?
              delete-database?: delete-database?
              refile?: refile?
              no-update-count?: no-update-count?
              mime?: mime?
              html?: html?
              proportional-probability?: prop-prob?
              naive-bayes?: naive-bayes?
              robinson?: robinson?
              db-file: db-file
              offset: offset
              key-size: key-size
              value-size: value-size
              min-length: min-length
              num-significant: num-significant
              chain-length: chain-length
              threshold: threshold
              weight-factor: weight-factor
              epsilon: epsilon)))
        (exit (if (and exit-code? (> (mstats-prob a) threshold)) 1 0))))))

;; run main
(main (cons "hato-classify" (command-line-arguments)))

