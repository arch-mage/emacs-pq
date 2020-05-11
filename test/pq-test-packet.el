;;; pq-test-packet --- PQ packet testing -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(require 'pq-packet)

(ert-deftest pq-test-packet-decode-authentication-ok ()
  (should
   (equal
    '(?R 0)
    (pq--decode-packet "R\x00\x00\x00\x08\x00\x00\x00\x00"))))

(ert-deftest pq-test-packet-decode-data-row ()
  (should
   (equal
    '(?D . ["1" "a" "true" "false" nil])
    (pq--decode-packet "D\x00\x00\x00\x25\x00\x05\ \x00\x00\x00\x01\ 1\x00\x00\x00\x01\ a\x00\x00\x00\x04\ true\x00\x00\x00\x05\ false\xff\xff\xff\xff"))))

(ert-deftest pq-test-packet-decode-error-response ()
  (should
   (equal
    '(?E . ((S . "FATAL")
            (C . "42601")
            (M . "Syntax error")))
    (pq--decode-packet "E\x00\x00\x00\x21SFATAL\x00\ C42601\x00\ MSyntax error\x00\x00"))))

(ert-deftest pq-test-packet-decode-backend-key-data ()
  (should
   (equal
    '(?K . [1 2])
    (pq--decode-packet "K\x00\x00\x00\x0c\x00\x00\x00\x01\x00\x00\x00\x02"))))

(ert-deftest pq-test-packet-decode-paramater-status ()
  (should
   (equal
    '(?S . ["user" "emacs"])
    (pq--decode-packet "S\x00\x00\x00\x0f\ user\x00\ emacs\x00"))))

(ert-deftest pq-test-packet-decode-row-description ()
  (should
   (equal
    '(?T . [["id" 1 1 23 4 0 0]
            ["name" 1 2 25 -1 0 0]
            ["active" 1 3 16 1 0 0]])
    (pq--decode-packet
     (concat "T\x00\x00\x00\x4b"
             "\x00\x03"
	     "id\x00\ \x00\x00\x00\x01\ \x00\x01\ \x00\x00\x00\x17\ \x00\x04\ \x00\x00\x00\x00\ \x00\x00"
	     "name\x00\ \x00\x00\x00\x01\ \x00\x02\ \x00\x00\x00\x19\ \xff\xff\ \x00\x00\x00\x00\ \x00\x00"
	     "active\x00\ \x00\x00\x00\x01\ \x00\x03\ \x00\x00\x00\x10\ \x00\x01\ \x00\x00\x00\x00\ \x00\x00")))))

(provide 'pq-test-packet)
;;; pq-test-packet.el ends here
