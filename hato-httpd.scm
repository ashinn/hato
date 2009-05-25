;; hato-httpd.scm -- a web server
;;
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(require-library
 srfi-1 srfi-13 srfi-69 tcp lolevel matchable sendfile let-args
 hato-config hato-uri hato-utils hato-log hato-daemon hato-mime hato-http)

(module hato-httpd (main)

(import scheme chicken extras ports regex posix files data-structures)
(import srfi-1 srfi-13 srfi-69 tcp lolevel matchable sendfile let-args)
(import hato-config hato-uri hato-utils hato-log hato-daemon hato-mime)
(import hato-http)

(define-logger (current-log-level set-current-log-level! info)
  (log-emergency log-alert log-critical log-error
   log-warn log-notice log-info log-debug))

(define *program-name* "hato-httpd")
(define-syntax read-version
  (er-macro-transformer
   (lambda (e r c) (call-with-input-file "VERSION" read-line))))
(define *program-version* (read-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This looks big and hairy, but it's mutation-free and guarantees:
;;   (string=? s (path-normalize s))  <=>  (eq? s (path-normalize s))
;; i.e. fast and simple for already normalized paths.

(define (path-normalize path)
  (let* ((len (string-length path)) (len-1 (- len 1)))
    (define (collect i j res)
      (if (>= i j) res (cons (substring path i j) res)))
    (define (finish i res)
      (if (zero? i)
        path
        (apply string-append (reverse (collect i len res)))))
    ;; loop invariants:
    ;;   - res is a list such that (string-concatenate-reverse res)
    ;;     is always the normalized string up to j
    ;;   - the tail of the string from j onward can be concatenated to
    ;;     the above value to get a partially normalized path referring
    ;;     to the same location as the original path
    (define (inside i j res)
      (if (>= j len)
        (finish i res)
        (if (eqv? #\/ (string-ref path j))
          (boundary i (+ j 1) res)
          (inside i (+ j 1) res))))
    (define (boundary i j res)
      (if (>= j len-1)
        (finish i res)
        (case (string-ref path j)
          ((#\.)
           (case (string-ref path (+ j 1))
             ((#\.)
              (if (or (>= j (- len 2)) (eqv? #\/ (string-ref path (+ j 2))))
                (if (>= i (- j 1))
                  (if (null? res)
                    (backup j "" '())
                    (backup j (car res) (cdr res)))
                  (backup j (substring path i j) res))
                (inside i (+ j 2) res)))
             ((#\/)
              (if (= i j)
                (boundary (+ j 2) (+ j 2) res)
                (let ((s (substring path i j)))
                  (boundary (+ j 2) (+ j 2) (cons s res)))))
             (else (inside i (+ j 1) res))))
          ((#\/) (boundary (+ j 1) (+ j 1) (collect i j res)))
          (else (inside i (+ j 1) res)))))
    (define (backup j s res)
      (let ((pos (+ j 3)))
        (cond
          ;; case 1: we're reduced to accumulating parents of the cwd
          ((or (string=? s "/..") (string=? s ".."))
           (boundary pos pos (cons "/.." (cons s res))))
          ;; case 2: the string isn't a component itself, skip it
          ((or (string=? s "") (string=? s ".") (string=? s "/"))
           (if (pair? res)
             (backup j (car res) (cdr res))
             (boundary pos pos (if (string=? s "/") '("/") '("..")))))
          ;; case3: just take the directory of the string
          (else
           (let ((d (pathname-directory s)))
             (cond
               ((string=? d "/")
                (boundary pos pos (if (null? res) '("/") res)))
               ((string=? d ".")
                (boundary pos pos res))
               (else (boundary pos pos (cons "/" (cons d res))))))))))
    ;; start with boundary if abs path, otherwise inside
    (if (zero? len)
      path
      ((if (eqv? #\/ (string-ref path 0)) boundary inside) 0 1 '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (http-respond status msg . headers)
  (printf "HTTP/1.1 ~S ~A\r\n" status msg)
  (if (pair? headers)
      (for-each
       (lambda (h)
         (display (car h)) (display ": ") (display (cdr h)) (display "\r\n"))
       (car headers)))
  (display "\r\n"))

(define http-mime-type
  (let ((types (make-hash-table eq?)))
    (condition-case
        (call-with-input-file (if (file-exists? "/etc/mime.types")
                                  "/etc/mime.types"
                                  "/etc/httpd/mime.types")
          (lambda (in)
            (let lp ()
              (let ((line (read-line in)))
                (cond
                 ((not (eof-object? line))
                  (let ((ls (string-split
                             (cond ((string-index line #\#)
                                    => (lambda (i) (substring line 0 i)))
                                   (else line)))))
                    (if (and (pair? ls) (pair? (cdr ls)))
                        (for-each
                         (lambda (x)
                           (hash-table-set! types (string->symbol x) (car ls)))
                         (cdr ls)))
                    (lp))))))))
      (exn () #f))
    (lambda (file)
      (let* ((ext (pathname-extension file))
             (mtype (or (and ext (hash-table-ref/default
                                  types
                                  (string->symbol (string-downcase ext))
                                  #f))
                        "application/octet-stream")))
        (if (equal? mtype "text/html")
            (string-append mtype "; charset=UTF-8")
            mtype)))))

(define (http-base-headers config)
  `((Server . ,*program-name*)
    (Connection . close)
    (Date . ,(seconds->string (current-seconds)))))

(define (http-file-headers file config)
  (append (http-base-headers config)
          `((Content-Length . ,(file-size file))
            (Content-Type . ,(http-mime-type file)))))

(define (http-send-file file request headers config)
  (let ((fd (condition-case (file-open file open/rdonly) (exn () #f))))
    (cond
     (fd
      (http-respond 200 "OK" (http-file-headers file config))
      (condition-case (sendfile fd (current-output-port)) (exn () #f))
      (file-close fd))
     (else ;; don't provide any more info in this case
      (http-respond 404 "File not found" '())))))

(define (http-document-root file uri headers config)
  (or (conf-get config 'document-root)
      (string-append (current-directory) "/www")))

(define (http-resolve-file file uri headers config)
  (string-append
   (http-document-root file uri headers config) "/"
   (string-trim (path-normalize file)
                (lambda (c) (if (eqv? c #\.) #t (eqv? c #\/))))))

(define (http-send-directory vdir uri headers config)
  (cond
   ((find (lambda (f) (file-exists? (string-append vdir "/" f)))
          (conf-get-list config 'index-files))
    => (lambda (f)
         (let ((uri2 (uri-with-path uri (string-append (uri-path uri) "/" f))))
           (http-handle-get uri2 headers config config))))
   (else
    (http-respond 200 "OK" (append (http-base-headers config)
                                   '((Content-Type . "text/html"))))
    (display "<html><body bgcolor=white><pre>\n")
    (let ((dir (string-trim (pathname-directory (uri-path uri)) #\/)))
      (for-each
       (lambda (file) (print "<a href=\"" dir "/" file "\">" file "</a>"))
       (directory vdir)))
    (display "</pre></body></html>\n"))))

(define (char-uri? ch)
  (<= 33 (char->integer ch) 126))

(define (string-match-substitute m subst str)
  (let lp ((ls subst) (res '()))
    (cond
     ((null? ls) (string-concatenate-reverse res))
     ((number? (car ls)) (lp (cdr ls) (cons (or (list-ref m (car ls)) "") res)))
     (else (lp (cdr ls) (cons (car ls) res))))))

(define (http-handle-get uri headers config vconfig)
  (let ((path (uri-path uri)))
    (cond
     ;; 0) sanity checks
     ((> (string-length path) 4096)
      (log-warn "long request: ~S" (substring path 0 4096))
      (http-respond 414 "Request-URI Too Long" '()))
     ((not (string-every char-uri? path))
      (log-warn "bad request: ~S" path)
      (http-respond 400 "Bad Request" '()))
     ;; 1) dispatch on rewrite rules
     ((any (lambda (x) (cond ((string-match (car x) path)
                         => (lambda (m) (list x m)))
                        (else #f)))
           (conf-multi config 'rewrite))
      => (match-lambda
             (((rx subst . flags) m)
              (log-notice "redirecting: ~S ~S" path m)
              (let ((new-path (string-match-substitute m subst path)))
                (cond
                 ((memq 'redirect flags)
                  (log-notice "redirecting => ~S" new-path)
                  (http-respond 302 "Moved Temporarily"
                                `((Location . ,new-path))))
                 (else
                  (log-notice "internal redirecting => ~S" new-path)
                  (http-handle-request (list 'GET new-path "HTTP/1.1")
                                       headers ;; XXXX adjust host
                                       config)))))
           (rule
            (log-error "bad rewrite rule: ~S => ~S" path rule)
            (http-respond 500 "Internal System Error"))))
     (else
      ;; 2) determine actual file in virtual directory
      (let ((vfile (http-resolve-file path uri headers vconfig)))
        (log-notice "GET ~S ~S" vfile headers)
        (let ((vconfig (condition-case
                           (conf-load
                            (string-append (pathname-directory vfile)
                                           "/.config")
                            config)
                         (exn () vconfig))))
          ;; 3) verify permissions
          (cond
           (#f
            (log-warn "no permissions")
            (http-respond 401 "Unauthorized" '()))
           ;; 4) determine handler from vdir, extension
           (else
            (if (directory? vfile)
                (http-send-directory vfile uri headers vconfig)
                (case (conf-get (list (conf-multi vconfig 'handlers))
                                (string->symbol
                                 (or (pathname-extension vfile) "xxx")))
                  ((private)
                   (log-warn "trying to access private file: ~S" vfile)
                   (http-respond 404 "Not Found" '()))
                  ((scheme)
                   ((load-scheme-script vfile vconfig)
                    vfile uri headers vconfig))
                  ((cgi)
                   (system vfile))
                  (else                 ; static file
                   (http-send-file vfile uri headers vconfig))))))))))))

(define (http-handle-head uri headers config vconfig)
  (http-handle-get uri headers config vconfig))

(define (http-handle-post uri headers config vconfig)
  (http-handle-get uri headers config vconfig))

(define (black-listed? ipaddr config)
  (assoc ipaddr (conf-multi config 'black-list)))

(define scheme-script-cache (make-hash-table))

(define hash-table-ref/cache!
  (let ((miss (list 'miss)))
    (lambda (tab key thunk)
      (let ((x (hash-table-ref/default tab key miss)))
        (if (eq? x miss)
            (let ((res (thunk)))
              (hash-table-set! tab key res)
              res)
            x)))))

(define (hash-table-ref/load! tab path proc)
  (let* ((mtime (file-modification-time path))
         (cell (hash-table-ref/cache!
                tab
                path
                (lambda () (cons mtime (proc path))))))
    (if (> mtime (car cell))
        (let ((res (proc path)))
          (hash-table-set! tab path (cons mtime res))
          res)
        (cdr cell))))

(define (%load-scheme-script path config)
  (call-with-input-file path
    (lambda (in)
      (cond ((eqv? #\# (peek-char in))
             (read-char in)
             (if (eqv? #\! (peek-char in)) (read-line in))))
      (let* ((modname (string-append "modscheme-" path))
             (handler-name (string-append modname "#handle"))
             (body `(module ,(string->symbol modname) (handle)
                      (import scheme chicken)
                      ,@(read-file in))))
        (eval body)
        (global-ref (string->symbol handler-name))))))

(define (load-scheme-script path config)
  (hash-table-ref/load!
   scheme-script-cache
   path
   (lambda (path) (%load-scheme-script path config))))

(define (http-handle-request request headers config)
  (let* ((uri (or (string->path-uri 'http (cadr request) #t #t)
                  (make-uri 'http #f #f #f)))
         (host (cond ((or (and (uri? uri) (uri-host uri))
                          (mime-ref headers "Host"))
                      => string-downcase->symbol)
                     (else #f)))
         (uri (if (or (uri-host uri) (not host))
                  uri
                  (uri-with-host uri host)))
         (vconfig (cond
                   ((assq-ref (conf-multi config 'virtual-hosts) host)
                    => (lambda (x) (conf-extend x config)))
                   (else config))))
    (log-notice "request: ~S uri: ~S" request uri)
    (case (car request)
      ((GET)  (http-handle-get uri headers config vconfig))
      ((HEAD) (http-handle-head uri headers config vconfig))
      ((POST) (http-handle-post uri headers config vconfig))
      (else
       (log-error "unknown method: ~S" request)
       (http-respond 400 "Bad Request" '())))))

(define (make-http-handler config)
  (lambda ()
    (receive (local remote) (tcp-addresses (current-input-port))
      (cond
       ((black-listed? remote config)
        (log-warn "black-listed: ~S" remote)
        (http-respond 401 "Unauthorized" '()))
       (else
        (let* ((request (http-parse-request))
               (headers (mime-headers->list (current-input-port) 128)))
          (cond
           ((not (= 3 (length request)))
            (log-error "bad request: ~S" request)
            (http-respond 400 "Bad Request" '()))
           (else
            (http-handle-request request headers config)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-help . o)
  (display "usage: ") (display *program-name*) (display "[options ...]\n")
  (display
"
 -h, --help            print this message
 -V, --version         print version number
 -c, --config=<file>   specify config file
     --no-config       don't load any config file
 -r, --root=<dir>      specify document root (default `pwd`/www)
 -p, --port=<num>      specify TCP port to listen on (default 5555)
 -u, --user=<id>       specify user to run as
 -g, --group=<id>      specify group to run as
 -d, --debug           debug run, don't detach
 -k, --kill            kill a running process
"))

(define (show-version . o)
  (display *program-name*) (display " ")
  (display *program-version*) (newline))

(define (main args)
  (let-args args
      ((name "name=s" *program-name*)
       (help "help|h" => show-help)
       (version "version|V" => show-version)
       (confdir "config-dir=s"
                (or (getenv "HTTP_CONF_DIR")
                    (if (zero? (current-user-id))
                        (string-append "/etc/" name)
                        (string-append (getenv "HOME") "/" name))))
       (rcfile "config|c=s" (string-append confdir "/httpd.conf"))
       (docroot "root|r=s")
       (port "port|p=i")
       (user "user|u=s")
       (group "group|g=s")
       (norc? "no-config")
       (debug? "debug|d")
       (kill? "kill|k")
       (else (opt rest cont) (error "invalid option" opt)))
    (let* ((config
            (conf-extend
             (filter
              (lambda (x) (cadr x))
              `((debug? ,debug?)
                (user-id
                 ,(and user (or (string->number user)
                                (cond ((user-information user) => caddr)
                                      (else #f)))))
                (group-id
                 ,(and group (or (string->number group)
                                (cond ((group-information group) => caddr)
                                      (else #f)))))
                (document-root ,docroot)
                (port ,port)))
             (if (and (not norc?) (file-exists? rcfile))
                 (conf-load rcfile) 
                 '())))
           (pid-file (or (conf-get config 'pid-file)
                         (string-append name ".pid"))))

      ;; verify the config
      ;;(conf-verify
      ;; config
      ;; '())

      ;; run
      (cond
       (kill?
        (daemon-kill pid-file 'name: name))
       (debug?
        ((make-http-handler config)))
       (else
        (daemonize 'name: name
                   'pid-file: pid-file
                   'tcp-port: (conf-get config 'port 5556)
                   'user-id: (conf-get config 'user-id)
                   'group-id: (conf-get config 'group-id)
                   'tcp-debug?: (conf-get config 'debug?)
                   'tcp-handler: (make-http-handler config)
                   ))))))

)

(import hato-httpd)
(main (command-line-arguments))

