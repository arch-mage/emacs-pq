;;; pq-future --- PQ async utilities -*- lexical-binding: t -*-

;;; Commentary:

;; This is basically a simple implementation of javascript Promise.

;;; Code:

(require 'seq)
(require 'pq-deque)

(defun pq-future-p (value)
  (eq (type-of value) 'pq-future))

(defun pq-future-pending-p (future)
  (and (eq (type-of future) 'pq-future)
       (eq (aref future 1) 'pending)))

(defun pq-future-fulfilled-p (future)
  (and (eq (type-of future) 'pq-future)
       (eq (aref future 1) 'fulfilled)))

(defun pq-future-rejected-p (future)
  (and (eq (type-of future) 'pq-future)
       (eq (aref future 1) 'rejected)))

(defun pq-future-current-value (future)
  (aref future 3))

(defun pq-future-new (executor)
  (let ((future (make-record 'pq-future 3 nil)) resolve reject)
    (aset future 1 'pending)
    (aset future 2 (pq-deque 2))

    (setq resolve
          (lambda (val)
            (when (pq-future-pending-p future)

              (if (pq-future-p val)
                  (pq-future-catch (pq-future-then val resolve) reject)

                (aset future 1 'fulfilled)
                (aset future 3 val)

                ;; (pq-deque-each
                ;;  (aref future 2)
                ;;  (lambda (elem) (funcall (aref elem 0) val)))

                (let ((next (pq-deque-pop-front (aref future 2))))
                  (while next
                    (funcall (aref next 0) val)
                    (setq next (pq-deque-pop-front (aref future 2)))))))))

    (setq reject
          (lambda (err)
            (when (pq-future-pending-p future)
              (aset future 1 'rejected)
              (aset future 3 err)

              ;; (pq-deque-each
              ;;  (aref future 2)
              ;;  (lambda (elem) (funcall (aref elem 1) err)))

              (let ((next (pq-deque-pop-front (aref future 2))))
                (while next
                  (funcall (aref next 1) err)
                  (setq next (pq-deque-pop-front (aref future 2))))))))

    (condition-case err (funcall executor resolve reject)
      (error (funcall reject err)))

    future))

(defmacro pq-future (arglist &rest body)
  "Create a new `pq-future' object.

\(fn (RESOLVE REJECT) BODY...)"
  (declare (indent 1))
  `(pq-future-new (lambda (,@arglist) ,@body)))

(defun pq-future-then (future then)
  (pq-future (resolve reject)
    (cond
     ((pq-future-fulfilled-p future)
      (condition-case err (funcall resolve (funcall then (aref future 3)))
        (error (funcall reject err))))
     ((pq-future-rejected-p future)
      (funcall reject (aref future 3)))
     ((pq-future-pending-p future)
      (pq-deque-push-back
       (aref future 2)
       (vector
        (lambda (val)
          (condition-case err (funcall resolve (funcall then val))
            (error (funcall reject err))))
        (lambda (err)
          (funcall reject err))))))))

(defmacro pq-future:then (future arglist &rest body)
  (declare (indent 2))
  `(pq-future-then ,future (lambda (,@arglist) ,@body)))

(defun pq-future-catch (future handle)
  (pq-future (resolve reject)
    (cond
     ((pq-future-fulfilled-p future)
      (condition-case err (funcall resolve (aref future 3))
        (error
         (condition-case err2 (funcall resolve (funcall handle err))
           (funcall reject err2)))))
     ((pq-future-rejected-p future)
      (condition-case err (funcall resolve (funcall handle (aref future 3)))
        (error (funcall reject err))))
     ((pq-future-pending-p future)
      (pq-deque-push-back
       (aref future 2)
       (vector
        resolve
        (lambda (err)
          (condition-case err2 (funcall resolve (funcall handle err))
            (error (funcall reject err2))))))))))

(defmacro pq-future:catch (future arglist &rest body)
  (declare (indent 2))
  `(pq-future-catch ,future (lambda (,@arglist) ,@body)))

(defun pq-future-resolve (value)
  (pq-future-new (lambda (resolve _) (funcall resolve value))))

(defun pq-future-chain (future &rest chains)
  (seq-reduce
   (lambda (future chain) (funcall chain future))
   chains
   future))

(defun pq-future-wait (secs future &optional max)
  (let ((counter 0))
    (while (eq (aref future 1) 'pending)
      (when (and max (>= counter max))
        (error "Future still not resolved"))
      (setq counter (1+ counter))
      (sit-for secs))
    (aref future 3)))

(provide 'pq-future)
;;; pq-future.el ends here
