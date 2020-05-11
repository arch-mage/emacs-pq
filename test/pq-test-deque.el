;;; pq-test-deque --- PQ deque testing -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(require 'pq-deque)

(ert-deftest pq-test-deque-length ()
  (should (= 0 (pq-deque-length (pq-deque))))
  (should (= 0 (pq-deque-length (pq-deque 2))))
  (should (= 2
             (let ((deque (pq-deque)))
               (pq-deque-push-back deque 1)
               (pq-deque-push-front deque 1)
               (pq-deque-length deque))))
  (should (= 2
             (let ((deque (pq-deque)))
               (pq-deque-push-front deque 1)
               (pq-deque-push-back deque 1)
               (pq-deque-length deque)))))

(ert-deftest pq-test-deque-back ()
  (should (= 1
             (let ((deque (pq-deque)))
               (pq-deque-push-back deque 1)
               (pq-deque-pop-back deque))))
  (should (= 1
             (let ((deque (pq-deque)))
               (pq-deque-push-back deque 0)
               (pq-deque-push-back deque 1)
               (pq-deque-pop-back deque))))
  (should (= 1
             (let ((deque (pq-deque)))
               (pq-deque-push-front deque 1)
               (pq-deque-push-front deque 0)
               (pq-deque-pop-back deque))))
  (should (= 1
             (let ((deque (pq-deque)))
               (pq-deque-push-back deque 1)
               (pq-deque-push-front deque 0)
               (pq-deque-pop-back deque))))
  (should (= 1
             (let ((deque (pq-deque)))
               (pq-deque-push-front deque 0)
               (pq-deque-push-back deque 1)
               (pq-deque-pop-back deque)))))

(ert-deftest pq-test-deque-front ()
  (should (= 1
             (let ((deque (pq-deque)))
               (pq-deque-push-back deque 1)
               (pq-deque-pop-front deque))))
  (should (= 1
             (let ((deque (pq-deque)))
               (pq-deque-push-back deque 1)
               (pq-deque-push-back deque 0)
               (pq-deque-pop-front deque))))
  (should (= 1
             (let ((deque (pq-deque)))
               (pq-deque-push-front deque 0)
               (pq-deque-push-front deque 1)
               (pq-deque-pop-front deque))))
  (should (= 1
             (let ((deque (pq-deque)))
               (pq-deque-push-back deque 0)
               (pq-deque-push-front deque 1)
               (pq-deque-pop-front deque))))
  (should (= 1
             (let ((deque (pq-deque)))
               (pq-deque-push-front deque 1)
               (pq-deque-push-back deque 0)
               (pq-deque-pop-front deque)))))

(provide 'pq-test-deque)
;;; pq-test-deque.el ends here
