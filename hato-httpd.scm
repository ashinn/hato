;; hato-httpd.scm -- a web server
;;
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(require-library srfi-1 srfi-13 srfi-69 tcp matchable sendfile let-args)
(require-library hato-config hato-uri hato-utils hato-log hato-daemon hato-mime)

(module hato-httpd (main)

(import scheme chicken extras ports regex posix files data-structures)
(import srfi-1 srfi-13 srfi-69 tcp matchable sendfile let-args)
(import hato-config hato-uri hato-utils hato-log hato-daemon hato-mime)

(define-logger (current-log-level set-current-log-level! info)
  (log-emergency log-alert log-critical log-error
   log-warn log-notice log-info log-debug))

(define *program-name* "hato-httpd")
(define-syntax read-version
  (er-macro-transformer
   (lambda (e r c) (call-with-input-file "VERSION" read-line))))
(define *program-version* (read-version))

(define (http-parse-request . o)
  (let ((line (string-split
               (read-line (if (pair? o) (car o) (current-input-port)) 4096))))
    (cons (string->symbol (car line)) (cdr line))))

(define (http-respond status msg headers)
  (printf "HTTP/1.1 ~S ~A\r\n" status msg)
  (for-each
   (lambda (h)
     (display (car h)) (display ": ") (display (cdr h)) (display "\r\n"))
   headers)
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
  (string-append (http-document-root file uri headers config) "/" file))

(define (http-send-directory dir uri headers config)
  (http-respond 200 "OK" (append (http-base-headers config)
                                 '((Content-Type . "text/html"))))
  (display "<html><body bgcolor=white><pre>\n")
  (for-each
   (lambda (file) (print "<a href=\"" dir "/" file "\">" file "</a>"))
   (directory dir))
  (display "</pre></body></html>\n"))

(define (char-uri? ch)
  (<= 33 (char->integer ch) 126))

(define (string-match-substitute m subst str)
  (let lp ((ls subst) (res '()))
    (cond
     ((null? ls) (string-concatenate-reverse res))
     ((number? (car ls)) (lp (cdr ls) (cons (list-ref m (car ls)) res)))
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
     ((find (lambda (x) (cond ((string-match (car x) path)
                          => (lambda (m) (list x m)))
                         (else #f)))
            (conf-multi config 'rewrite))
      => (match-lambda
             (((rx subst . flags) m)
              (let ((new-path (string-match-substitute m subst path)))
                (cond
                 ((memq 'redirect flags)
                  (log-notice "redirecting => ~S" new-path)
                  (http-respond 302 "Moved Temporarily"
                                `((Location . ,new-path))))
                 (else
                  (http-handle-request (cons 'GET new-path "HTTP/1.1")
                                       headers ;; XXXX adjust host
                                       config)))))))
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
                (case (assoc-ref (conf-multi config 'handlers)
                                 (pathname-extension vfile))
                  ((private)
                   (log-warn "trying to access private file: ~S" vfile)
                   (http-respond 404 "Not Found" '()))
                  ((scheme)
                   ((load-scheme-script vfile) vfile uri headers vconfig))
                  ((cgi)
                   #f)
                  (else ; static file
                   (http-send-file vfile uri headers vconfig))))))))))))

(define (http-handle-head uri headers config vconfig)
  #f)

(define (http-handle-post uri headers config vconfig)
  #f)

(define (black-listed? ipaddr config)
  (assoc ipaddr (conf-multi config 'black-list)))

(define (load-scheme-script path)
  #f)

(define (http-handle-request request headers config)
  (let* ((uri (or (string->path-uri 'http (cadr request))
                  (make-uri 'http #f #f #f)))
         (host (cond ((or (and (uri? uri) (uri-host uri))
                          (mime-ref headers "Host"))
                      => string-downcase->symbol)
                     (else #f)))
         (vconfig (cond
                   ((assq-ref (conf-multi config 'virtual-hosts) host)
                    => (lambda (x) (conf-extend x config)))
                   (else config))))
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
       (rcfile "config|c=s" (string-append confdir "/httpd.config"))
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
                   'tcp-port: (conf-get config 'port 5555)
                   'user-id: (conf-get config 'user-id)
                   'group-id: (conf-get config 'group-id)
                   'tcp-debug?: (conf-get config 'debug?)
                   'tcp-handler: (make-http-handler config)
                   ))))))

)

(import hato-httpd)
(main (command-line-arguments))

