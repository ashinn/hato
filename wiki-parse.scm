
(require-library regex srfi-1 srfi-13)

(module wiki-parse (wiki-parse wiki-word-encode wiki-word-decode)

(import scheme chicken extras irregex data-structures srfi-1 srfi-13)

;; translate a wiki word to a safe pathname (after escaping, colons
;; ':' are translated to slashes '/' to create a diretory hierarchy of
;; wiki words)
(define wiki-word-encode
  (let ((wiki-unsafe-rx (irregex "[^!--0-Z^-~]")))
    (lambda (str)
      (string-translate
       (irregex-replace/all
        wiki-unsafe-rx
        (string-downcase str)
        (lambda (m)
          (let ((n (char->integer (string-ref (irregex-match-substring m) 0))))
            (string-append "_" (if (< n 16) "0" "")
                           (string-upcase (number->string n 16))))))
       ":"
       "/"))))

;; reverse the above
(define wiki-word-decode
  (let ((wiki-escaped-rx (irregex "_([0-9a-fA-F][0-9a-fA-F])")))
    (lambda (str)
      (irregex-replace/all
       wiki-escaped-rx
       (string-translate str "/" ":")
       (lambda (m)
         (string
          (integer->char
           (string->number (irregex-match-substring m 1) 16))))))))

(define (%irregex-multi-fold ls str start end)
  ;;(fprintf (current-error-port) "irregex-multi-fold ~S ~S-~S\n"
  ;;         (length ls) start end)
  (cond
   ((null? ls)
    (if (>= start end) '() (list (substring str start end))))
   (else
    (irregex-fold
     (caar ls)
     (lambda (i m x)
       (let ((left (%irregex-multi-fold (cdr ls) str i (irregex-match-start m)))
             (right (reverse (irregex-apply-match m (cdar ls)))))
         ;;(fprintf (current-error-port) "left: ~S right: ~S\n" left right)
         (append x left right)))
     '()
     str
     (lambda (i x)
       ;;(fprintf (current-error-port) "final: ~S ~S\n" i x)
       (append x (%irregex-multi-fold (cdr ls) str i end)))
     start
     end))))

(define (irregex-multi-fold ls str . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o)))
                 (cadr o)
                 (string-length str))))
    (%irregex-multi-fold ls str start end)))

(define wiki-parse-inline
  (let ((wiki-bold-rx
         (irregex "'''([^']+)'''")) ;;"\\*([^*]+)\\*"
        (wiki-italic-rx
         (irregex "''([^']+)''")) ;;"/([^/]+)/"
        (wiki-uline-rx
         (irregex "<u>([^<]+)</u>"))
        (wiki-strike-out-rx
         (irregex "<s>([^<]+)</s>"))
        (wiki-word-rx
         (irregex "\\[\\[([^\\]|]+)(?:\\| *([^\\]|]+))?\\]\\]"))
        (wiki-note-rx
         (irregex "\\{\\{([^\\}]+)\\}\\}")) ;;(?:\\| *([^\\}|]+))?
        (wiki-url-rx
         (irregex "((?:https?|ftp):/+[\\-+.,_/%?&~=:\\w]+[\\-+_/%?&~=:\\w])"))
        (wiki-named-url-rx
         (irregex "\\[((?:https?|ftp):/+[\\-+.,_/%?&~=:\\w]+[\\-+_/%?&~=:\\w])[ \t\n]+([^\\]]*)\\]")))
    (lambda (str)
      (irregex-multi-fold
       `((,wiki-word-rx
          ,(lambda (m)
             (list 'wiki
                   (irregex-match-substring m 1)
                   (wiki-word-encode
                    (or (irregex-match-substring m 2)
                        (irregex-match-substring m 1))))))
         (,wiki-note-rx
          ,(lambda (m)
             (list 'note
                   (wiki-parse-inline (irregex-match-substring m 1))
                   ;;(irregex-match-substring m 2)
                   #f)))
         (,wiki-named-url-rx
          ,(lambda (m)
             (list 'url
                   (irregex-match-substring m 1)
                   (irregex-match-substring m 2))))
         (,wiki-url-rx
          ,(lambda (m)
             (list 'url
                   (irregex-match-substring m 1)
                   (irregex-match-substring m 1))))
         (,wiki-bold-rx
          ,(lambda (m) (list 'b (irregex-match-substring m 1))))
         (,wiki-italic-rx
          ,(lambda (m) (list 'i (irregex-match-substring m 1))))
         (,wiki-uline-rx
          ,(lambda (m) (list 'u (irregex-match-substring m 1))))
         (,wiki-strike-out-rx
          ,(lambda (m) (list 's (irregex-match-substring m 1))))
         )
       str))))

(define wiki-parse
  (let ((wiki-hr-rx
         (irregex "^\\s*----+\\s*$"))
        (wiki-header-rx
         (irregex "^=(=+)([^=]+)=+\\s*$"))
        (wiki-list-level
         (lambda (str) (string-prefix-length "**********" str))))
    (lambda (src)
      (let ((in (if (string? src) (open-input-string src) src)))
        (let parse ((res '())
                    (par '())
                    (list-level 0))
          (define (collect)
            (cond
             ((null? par)
              res)
             ((> list-level 0)
              `((ul ,@(map (lambda (x) (cons 'li (wiki-parse-inline x)))
                           (reverse par)))
                ,@res))
             (else
              `((p
                 ,@(reverse (drop-while string? par))
                 ,@(wiki-parse-inline
                    (string-intersperse
                     (reverse (take-while string? par))
                     "\n")))
                ,@res))))
          (let ((line (read-line in)))
            (cond
             ((eof-object? line)
              (reverse (collect)))
             ((equal? "" line)
              (if (and (pair? par)
                       (pair? (car par))
                       (memq (caar par) '(h1 h2 h3 h4 h5 h6)))
                  (parse res par list-level)
                  (parse (collect) '() 0)))
             ((eqv? #\space (string-ref line 0))
              (let lp ((ls (list line)))
                (if (eqv? #\space (peek-char in))
                    (lp (cons (read-line in) ls))
                    (let* ((prefix
                            (fold (lambda (s p)
                                    (min (string-prefix-length
                                          (make-string 20 #\space)
                                          s)
                                         p))
                                  20 ls))
                           ;; strip up to 20 leading space chars
                           (ls (map (lambda (s) (substring s prefix)) ls)))
                      (parse
                       (cons (list 'code (string-intersperse (reverse ls) "\n"))
                             (collect))
                       '()
                       0)))))
             ((irregex-match wiki-hr-rx line)
              (parse (cons (list 'hr) (collect)) '() 0))
             ((irregex-match wiki-header-rx line)
              => (lambda (m)
                   (let* ((h (irregex-match-substring m 1))
                          (depth (number->string (min (string-length h) 6))))
                     (parse
                      (collect)
                      (list (list (string->symbol (string-append "h" depth))
                                  (irregex-match-substring m 2)))
                      0))))
             (else
              (let* ((level (wiki-list-level line))
                     (line (if (zero? level)
                               line
                               (string-trim (substring line level)))))
                (cond
                 ((= level list-level)
                  (parse res (cons line par) list-level))
                 (else
                  (parse (collect) (list line) level))))))))))))

)
