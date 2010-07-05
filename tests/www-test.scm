#! /usr/local/bin/csi -script

(use test posix srfi-1)

(define (ls-a dir)
  (map (cut string-append dir "/" <>)
       (sort (directory dir #t) string<?)))

(define (with-test-server-from-dir dir thunk)

  ;; clean out any previous test files (keep the log files around)
  (for-each
   (lambda (d) (if (directory? d) (for-each delete-file (ls-a d))))
   (map (cut string-append dir <>)
        '("/root/var/queue" "/root/var/run" "/root/var/mail")))
  ;; ... wipe out the homes completely (be careful with this!)
  (letrec ((rm-rf
            (lambda (f)
              (if (directory? f)
                  (for-each
                   rm-rf
                   (map (lambda (x) (string-append f "/" x))
                        (directory f #t)))
                  (delete-file f)))))
    (rm-rf (string-append dir "/root/home")))
  ;; ... and re-populate the homes
  (copy-directory-tree (string-append dir "/init-home")
                       (string-append dir "/root/home"))

  ;; start the server
  (test-assert
   (sprintf "starting server in ~A" dir)
   (zero?
    (system
     (sprintf
      "cd .. && ./hato-httpd --port ~A --root tests/~A/root/"
      test-port dir))))

  ;; do whatever
  (thunk)

  ;; kill the server
  (test-assert
   (sprintf "stopping server in ~A" dir)
   (zero?
    (system
     (sprintf "cd .. && ./hato-httpd --root tests/~A/root/ --kill"
              dir)))))

(test-begin)

(test-end)

