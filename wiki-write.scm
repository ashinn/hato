
(require-library srfi-1 srfi-13 highlight html-parser wiki-utils)

(module wiki-write (wiki-write)

(import scheme chicken extras regex srfi-1 srfi-13)
(import highlight html-parser wiki-utils)

(define (wiki-write-main x par note)
  (define (wiki-write x note) (wiki-write-main x par note))
  (cond
   ((pair? x)
    (case (car x)
      ((note)
       (cond
        ((and (string? (cadr x)) (string-prefix-ci? "image:" (cadr x)))
         note)
        (else
         (display "<span class=\"notelink\"><sup>[")
         (display note)
         (display "]</sup></span>")
         (+ note 1))))
      ((wiki)
       (display "<a href=\"") (display (cadr x))
       (display "\"")
       (cond
        ((and (pair? (cddr x)) (pair? (cdddr x)) (string? (cadddr x)))
         (display " onmouseover=\"javascript:shownote('doc") (display par)
         (display "', '") (display (cadddr x)) (display "')\"")
         ;;(display " onmouseout=\"javascript:hidenote('doc") (display par)
         ;;(display "')\"")
         ))
       (display ">")
       (display (wiki-word-display (caddr x)))
       (display "</a>")
       note)
      (else
       (cond
        ((symbol? (car x))
         (let* ((attrs? (and (pair? (cdr x)) (pair? (cadr x))
                             (eq? '@ (caadr x))))
                (attrs (if attrs? (cdadr x) '())))
           (display (html-tag->string (car x) attrs))
           (let ((note (fold wiki-write note (if attrs? (cddr x) (cdr x)))))
             (display "</") (display (car x)) (display ">")
             note)))
        (else
         (fold wiki-write note x))))))
   (else
    (display (if (string? x) (html-escape x) x))
    note)))

(define (wiki-write-notes x par note)
  (if (pair? x)
      (cond
       ((eq? 'note (car x))
        (cond
         ((and (string? (cadr x)) (string-prefix-ci? "image:" (cadr x)))
          (display "<img src=\"")
          (display (substring (cadr x) 6))
          (display "\" />")
          note)
         (else
          (display "<span class=\"notelink\">[")
          (display note)
          (display "]</span> ")
          (wiki-write-main (cadr x) par note)
          (+ note 1))))
       (else
        (wiki-write-notes (cdr x) par (wiki-write-notes (car x) par note))))
      note))

(define (wiki-write-row x par note)
  (display "<tr>\n<td id=\"doc") (display par)
  (display "\" class=\"doc\" width=\"200px\" ></td>\n")
  (display "<td class=\"main\">\n")
  (let ((note2 (wiki-write-main x par note)))
    (display "</td>\n<td class=\"note\">\n")
    (wiki-write-notes x par note)
    (display "\n</td>\n</tr>\n")
    note2))

(define (wiki-write x)
  (display "<table>\n")
  (fold wiki-write-row 1 x (iota (length x) 1))
  (display "</table>\n"))

)

