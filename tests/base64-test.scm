
(use test)

(load "hato-base64.scm")

(test-begin)

(test "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXo="
    (base64-encode-string "abcdefghijklmnopqrstuvwxyz"))

(test "abcdefghijklmnopqrstuvwxyz"
    (base64-decode-string "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXo"))
(test "abcdefghijklmnopqrstuvwxyz"
    (base64-decode-string "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXo="))
(test "abcdefghijklmnopqrstuvwxyz"
    (base64-decode-string "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXo=="))
(test "abcdefghijklmnopqrstuvwxyz"
    (base64-decode-string "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXo=YWJjZGVm="))
(test "abcdefghijklmnopqrstuvwxyz"
    (base64-decode-string "YWJjZ\nGVmZ2\nhpamtsbW5\nvcHFyc3R1dnd4eXo="))

(test "abcdefghijklmnopqrstuvwxyz"
    (with-output-to-string
      (lambda ()
        (base64-decode
         (open-input-string "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXo")))))
(test "abcdefghijklmnopqrstuvwxyz"
    (with-output-to-string
      (lambda ()
        (base64-decode
         (open-input-string "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXo=")))))
(test "abcdefghijklmnopqrstuvwxyz"
    (with-output-to-string
      (lambda ()
        (base64-decode
         (open-input-string "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXo==")))))
(test "abcdefghijklmnopqrstuvwxyz"
    (with-output-to-string
      (lambda ()
        (base64-decode
         (open-input-string "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXo=YWJjZGVm=")))))
(test "abcdefghijklmnopqrstuvwxyz"
    (with-output-to-string
      (lambda ()
        (base64-decode
         (open-input-string "YWJjZ\nGVmZ2\nhpamtsbW5\nvcHFyc3R1dnd4eXo=")))))

(test-end)

