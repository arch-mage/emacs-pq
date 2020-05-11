;;; pq-test-bindat --- PQ bindat testing -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(require 'pq-bindat)

(ert-deftest pq-test-bindat-pack ()
  (should
   (equal
    "ABCDEFGstring\x00\ A\x02\ BCkey\x00val\x00\ ABCDEF"
    (pq-bindat-pack
     '((n8     u8)
       (n16    u16)
       (n32    u32)
       (string cstr)
       (vecf   vec 1 u8)
       (count  u8)
       (vecn   vec (count) u8)
       (data   struct ((key cstr)
                       (val cstr)))
       (nums   [u8 u8])
       (bytes  bytes 2)
       (byten  bytes (count)))
     '((n8     . #x41)
       (n16    . #x4243)
       (n32    . #x44454647)
       (string . "string")
       (vecf   . [?A])
       (count  . 2)
       (vecn   . [?B ?C])
       (data   . ((key . "key")
                  (val . "val")))
       (nums   . [?A ?B])
       (bytes  . "CD")
       (byten  . "EF"))))))

(ert-deftest pq-test-bindat-unpack ()
  (should
   (equal
    '((n8     . #x41)
      (n16    . #x4243)
      (n32    . #x44454647)
      (string . "string")
      (vecf   . [?A])
      (count  . 2)
      (vecn   . [?B ?C])
      (data   . ((key . "key")
                 (val . "val")))
      (nums   . [?A ?B])
      (bytes  . "CD")
      (byten  . "EF"))
    (pq-bindat-unpack
     '((n8     u8)
       (n16    u16)
       (n32    u32)
       (string cstr)
       (vecf   vec 1 u8)
       (count  u8)
       (vecn   vec (count) u8)
       (data   struct ((key cstr)
                       (val cstr)))
       (nums   [u8 u8])
       (bytes  bytes 2)
       (byten  bytes (count)))
     "ABCDEFGstring\x00\ A\x02\ BCkey\x00val\x00\ ABCDEF"))))

(ert-deftest pq-test-bindat-pack-v ()
  (should
   (equal
    "ABCDEFGstring\x00"
    (pq-bindat-pack
     '[u8 u16 u32 cstr]
     '[#x41 #x4243 #x44454647 "string"]
     ))))

(ert-deftest pq-test-bindat-unpack-v ()
  (should
   (equal
    '[#x41 #x4243 #x44454647 "string"]
    (pq-bindat-unpack
     '[u8 u16 u32 cstr]
     "ABCDEFGstring\x00"
     ))))

(provide 'pq-test-bindat)
;;; pq-test-bindat.el ends here
