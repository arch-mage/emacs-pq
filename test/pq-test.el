;;; pq-test --- PQ test -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(require 'pq)

(ert-deftest pq-test-query ()
  (should
   (equal
    '[[("id" . "1") ("name" . "someone")]
      [("id" . "2") ("name" . "anyone")]]
    (pq-future-wait
     0.01
     (pq:with-temp-conn (list :host "127.0.0.1"
                              :port 5432
                              :user "emacs"
                              :password "emacs"
                              :database "emacs")
       (pq:exec "CREATE TEMP TABLE person (id SERIAL PRIMARY KEY, name TEXT)")
       (pq:exec '("INSERT INTO person (name) VALUES ($1), ($2)" "someone" "anyone"))
       (pq:then (_)
         (make-vector 2 nil))
       (pq:query "SELECT id, name FROM person ORDER BY id" (row columns idx result)
         (let ((zip (make-vector (length columns) nil)))
           (seq-map-indexed
            (lambda (col idx) (aset zip idx (cons (aref col 0) (aref row idx))))
            columns)
           (aset result idx zip)
           result)))
     100)
    )))

(provide 'pq-test)
;;; pq-test.el ends here
