
(require-library srfi-1 srfi-13 regex posix matchable
                 highlight hato-uri hato-http html-parser html-summary)

(module wiki-utils (wiki-sugar wiki-word-display)

(import scheme chicken extras regex data-structures files posix srfi-1 srfi-13)
(import matchable highlight hato-uri hato-http html-parser html-summary)

(define get-file-chunk-size 4096)

(define (get-file-cached-url url path)
  (if (not (file-exists? path))
      (call-with-input-url url
        (lambda (in)
          (let* ((buf (make-string get-file-chunk-size))
                 (out (open-output-file path))
                 (lock (file-lock out)))
            (let lp ()
              (let ((n (read-string! get-file-chunk-size buf in)))
                (write-string buf n out)
                (if (= n get-file-chunk-size)
                    (lp))))
            (file-unlock lock)
            (close-output-port out)))))
  path)

(define (transform-url-html proc url word words)
  (condition-case
      (let* ((uri (if (string? url) (string->path-uri 'http url) url))
             (prefix0 (uri->string (uri-with-path uri "/")))
             (prefix (substring prefix0 0 (- (string-length prefix0) 1)))
             (dir (pathname-directory (or (uri-path uri) "/"))))
        (sxml->html
         (html-adjust-relative
          (proc
           (call-with-input-file
               (get-file-cached-url url (string-append words "/" word ".cache"))
             html->sxml))
          prefix
          dir)))
    (exn () "")))

(define (get-title url word words)
  (transform-url-html html-title url word words))

(define (get-summary url word words)
  (transform-url-html html-summary url word words))

(define paren-rx (regexp "[ _]?\\([^)]+\\)"))

(define (wiki-word-display str)
  (string-substitute
   paren-rx
   ""
   (cond ((string-index-right str #\:) => (lambda (i) (substring str (+ i 1))))
         (else str))))

(define (wiki-code-links x words aliases)
  ;; XXXX handle other languages
  (let* ((r5rs (cond ((assoc "r5rs:" aliases) => cdr) (else '())))
         (prefix (and (pair? r5rs) (car r5rs)))
         (docs (if (pair? r5rs) (cdr r5rs) '())))
    (let lp ((x x))
      (if (not (pair? x))
          x
          (if (eq? 'span (car x))
              (let* ((attrs? (and (pair? (cdr x)) (pair? (cadr x))
                                  (eq? '@ (caadr x))))
                     (name (if attrs? (cddr x) (cdr x))))
                (if (and (pair? name) (every string? name))
                    (let ((cell (assq (string->symbol (string-concatenate name))
                                      docs)))
                      (if cell
                          `(span ,@(if attrs? (list (cadr x)) '())
                                 (wiki ,(string-append prefix (cadr cell))
                                       ,@name
                                       ,@(cddr cell)))
                          x))
                    (cons (lp (car x)) (lp (cdr x)))))
              (cons (lp (car x)) (lp (cdr x))))))))

(define (wiki-sugar x words aliases)
  (define (sugar x) (wiki-sugar x words aliases))
  (if (not (pair? x))
      x
      (case (car x)
        ((wiki)
         (let ((word (cadr x)))
           (cond
            ((find (lambda (a) (string-prefix-ci? (car a) word)) aliases)
             => (lambda (a)
                  (let ((rel-word (substring word (string-length (car a)))))
                    (cond
                     ((and (pair? (cddr a)) (pair? (caddr a)))
                      ;; inline docs like r5rs:
                      (let ((cell (assq (string->symbol rel-word) (cddr a))))
                        (if (not cell)
                            (list 'wiki (cadr a) rel-word)
                            (let* ((url (string-append (cadr a)
                                                       (or (cadr cell) "")))
                                   (doc (and (pair? (cddr cell))
                                             (caddr cell))))
                              (list 'wiki url rel-word
                                    (and (not (equal? "" doc)) doc))))))
                     (else
                      (let* ((url (string-append
                                   (cadr a)
                                   (string-translate rel-word " " "_")))
                             (doc (case (and (pair? (cddr a)) (caddr a))
                                    ((summary) (get-summary url word words))
                                    ((title) (get-title url word words))
                                    (else ""))))
                        (list 'wiki url rel-word
                              (and doc
                                   (not (equal? "" doc))
                                   (html-escape doc)))))))))
            ((string-prefix-ci? "image:" word)
             `(img (@ (src ,(substring word 6)))))
            (else
             x))))
        ((url)
         `(a (@ (href ,(cadr x))) ,(caddr x)))
        ((code)
         (let ((code (html->sxml (highlight (cadr x)))))
           `(code (pre ,(wiki-code-links code words aliases)))))
        (else
         (cond
          ((pair? (car x))
           (map sugar x))
          ((and (pair? (cdr x)) (pair? (cadr x)) (eq? '@ (caadr x)))
           (cons (car x) (cons (cadr x) (map sugar (cddr x)))))
          (else
           (cons (car x) (map sugar (cdr x)))))))))

)

