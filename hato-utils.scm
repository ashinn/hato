
(require-library posix srfi-13 srfi-18 stty hato-log)

(module hato-utils
    (get-user-home expand-user-path
     port-open? copy-port create-directory*
     read-password fifo-clear exception-message
     die die-with-exit-code
     with-timeout
     )

(import scheme chicken extras files posix srfi-13 srfi-18 stty hato-log
        (only safe-io current-safe-user-id current-safe-group-id))

(define (with-timeout timeout thunk . o)
  (let* ((th (make-thread thunk))
         (timeout-val (list 'timeout))
         (res (thread-join! (thread-start! th) timeout timeout-val)))
    (if (eq? res timeout-val)
      (if (pair? o)
        ((car o))
        (error "timeout exceeded" timeout))
      res)))

(define (get-user-home user)
  (let ((info (user-information user)))
    (and info (caddr (cdddr info)))))

(define (expand-user-path user path . o)
  (let ((get-home (if (pair? o) (car o) get-user-home))
        (cwd (and (pair? o) (pair? (cdr o)) (cadr o))))
    (cond
     ((equal? path "")
      (if (pair? o) (car o) (get-home user)))
     ((eqv? #\~ (string-ref path 0))
      (let ((slash (string-index path #\/)))
        (cond
         ((eqv? 1 slash)
          (string-append (get-home user) (substring path 1)))
         (slash
          (string-append (get-home (substring path 1 slash))
                         (substring path slash)))
         (else
          (string-append (get-home (substring path 1)) "/")))))
     ((eqv? #\/ (string-ref path 0))
      path)
     (else
      (let ((dir (or cwd (get-home user))))
        (string-append
         (if (eqv? "~" (string-ref dir 0))
             (expand-user-path user dir)
             dir)
         (if (and (not (equal? "" dir))
                  (not (eqv? #\/ (string-ref dir (- (string-length dir) 1)))))
             "/"
             "")
         path))))))

(define (port-open? port)
  (and (port? port)
       (not (##sys#slot port 8))))

(define (copy-port in out)
  (let lp ()
    (let ((str (read-string 1024 in)))
      (if (not (equal? str ""))
          (begin
            (write-string str #f out)
            (lp))))))

(define (create-directory* dir . o)
  (let create ((dir dir) (limit (if (pair? o) (car o) 10)))
    (condition-case (begin (create-directory dir) #t)
      (exn ()
           (or
            (and (positive? limit)
                 (let ((parent
                        (pathname-directory
                         (string-trim-right dir))))
                   (and (not (string=? parent ""))
                        (not (string=? parent "/"))
                        (not (file-exists? parent))
                        (create parent (- limit 1))
                        (begin
                          (create-directory dir)
                          #t))))
            (signal exn))))))

(define (read-password prompt . o)
  (let ((verify (if (pair? o) (car o) (lambda (x) #t))))
    (let lp ((count 3))
      (cond
       ((zero? count)
        #f)
       (else
        (display prompt)
        (let ((pass (with-stty '(not echo) read-line)))
          (newline)
          (let ((res (verify pass)))
            (if res
                pass
                (lp (- count 1))))))))))

(define fifo-clear
  (let ((buf (make-string 1)))
    (lambda (fd . o)
      (receive (in? out?) (apply file-select fd #f o)
        (let ((res (and in? (file-read fd 1 buf))))
          (if (and in? (= 1 (cadr res)))
              (fifo-clear fd)))))))

(define exception-message
  (let ((get-msg (condition-property-accessor 'exn 'message))
        (get-args (condition-property-accessor 'exn 'arguments)))
    (lambda (exn) (sprintf "~A ~S" (get-msg exn) (get-args exn)))))

(define (die-with-exit-code n fmt . args)
  (let ((msg (apply sprintf fmt args))
        (out (if (and (output-port? (current-error-port))
                      (port-open? (current-error-port)))
                 (current-error-port)
                 current-log-port)))
    (display msg out)
    (if (not (eqv? #\newline (string-ref msg (- (string-length msg) 1))))
        (newline out)))
  (exit n))

(define (die . args)
  (apply die-with-exit-code (if (number? (car args)) args (cons 1 args))))

)

