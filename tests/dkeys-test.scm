#!/usr/local/bin/csi -script

(use domain-keys posix test)

(test-begin)

(test 'valid
    (domain-key-verify (with-input-from-file "message.txt" read-all)))

(test-end)
