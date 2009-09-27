
(use test hato-md5)

(test-begin "md5")

(test "empty" "d41d8cd98f00b204e9800998ecf8427e"
  (md5-digest ""))

(test "a" "0cc175b9c0f1b6a831c399e269772661"
  (md5-digest "a"))

(test "abc" "900150983cd24fb0d6963f7d28e17f72"
  (md5-digest "abc"))

(test "message digest" "f96b697d7cb7938d525a2f31aaf161d0"
  (md5-digest "message digest"))

(test "lowercase alphabet" "c3fcd3d76192e4007dfb496cca67e13b"
  (md5-digest "abcdefghijklmnopqrstuvwxyz"))

(test "alphanumeric" "d174ab98d277d9f5a5611c2c9f419d9f"
  (md5-digest "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))

(test "many digits" "57edf4a22be3c955ac49da2e2107b67a"
  (md5-digest "12345678901234567890123456789012345678901234567890123456789012345678901234567890"))

(test-end)
