;;; pq-test-future --- PQ future testing -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'pq-future)

(ert-deftest pq-test-future-sync-resolve ()
  (should
   (equal
    "OK"
    (aref (pq-future-new (lambda (resolve _) (funcall resolve "OK"))) 3))))

(ert-deftest pq-test-future-sync-reject ()
  (should
   (equal
    "err"
    (aref (pq-future-new (lambda (_ reject) (funcall reject "err"))) 3))))

(ert-deftest pq-test-future-sync-resolve-err ()
  (should
   (equal
    '(error "err")
    (aref (pq-future-new (lambda (_ok _err) (error "err"))) 3))))

(ert-deftest pq-test-future-async-resolve ()
  (let (future)
    (setq
     future
     (pq-future-new
      (lambda (resolve _)
        (run-at-time 0.01 nil (lambda () (funcall resolve "OK"))))))
    (should
     (equal
      "OK"
      (pq-future-wait 0.02 future)))))

(ert-deftest pq-test-future-async-reject ()
  (let (future)
    (setq
     future
     (pq-future-new
      (lambda (_ reject)
        (run-at-time 0.01 nil (lambda () (funcall reject "err"))))))
    (should
     (equal
      "err"
      (pq-future-wait 0.02 future)))))

(ert-deftest pq-test-future-then()
  (should
   (equal
    2
    (pq-future-wait
     0.02
     (pq-future-then
      (pq-future-new
       (lambda (resolve _)
         (run-at-time 0.01 nil (lambda () (funcall resolve 1)))))
      (lambda (val) (1+ val)))))))

(ert-deftest pq-test-future-catch ()
  (should
   (equal
    "handled: err"
    (pq-future-wait
     0.02
     (pq-future-catch
      (pq-future-new
       (lambda (_ reject)
         (run-at-time 0.01 nil (lambda () (funcall reject "err")))))
      (lambda (err) (format "handled: %s" err))))))

  (should
   (equal
    "handled: (error err)"
    (pq-future-wait
     0.02
     (pq-future-catch
      (pq-future-then
       (pq-future-new
        (lambda (resolve _)
          (run-at-time 0.01 nil (lambda () (funcall resolve "OK")))))
       (lambda (_) (error "err")))
      (lambda (err) (format "handled: %s" err))))))

  (should
   (equal
    "then: handled: (error err)"
    (pq-future-wait
     0.02
     (pq-future-then
      (pq-future-catch
       (pq-future-then
        (pq-future-new
         (lambda (resolve _)
           (run-at-time 0.01 nil (lambda () (funcall resolve "OK")))))
        (lambda (_) (error "err")))
       (lambda (err) (format "handled: %s" err)))
      (lambda (val) (format "then: %s" val)))))))

(provide 'pq-test-future)
;;; pq-test-future.el ends here
