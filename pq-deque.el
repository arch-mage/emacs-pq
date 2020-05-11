;;; pq-deque --- deque data structure -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun pq-deque (&optional size)
  "Create a new deque with initial SiZE.

SIZE must be greater than 1."
  (when (and size (< size 2))
    (error "Initial size of deque must be greater than 1"))
  (record 'pq-deque 0 0 (make-vector (or (and size (1+ size)) 2) nil)))

(defun pq-deque-move-head-forward (deque)
  "Move head pointer in DEQUE forward."
  (aset deque 1 (mod (1+ (aref deque 1)) (length (aref deque 3)))))

(defun pq-deque-move-tail-forward (deque)
  "Move tail pointer in DEQUE forward."
  (aset deque 2 (mod (1+ (aref deque 2)) (length (aref deque 3)))))

(defun pq-deque-move-head-backward (deque)
  "Move head pointer in DEQUE backward."
  (aset deque 1 (mod (1- (aref deque 1)) (length (aref deque 3)))))

(defun pq-deque-move-tail-backward (deque)
  "Move tail pointer in DEQUE backward."
  (aset deque 2 (mod (1- (aref deque 2)) (length (aref deque 3)))))

(defun pq-deque-full-p (deque)
  "Return non nil if DEQUE is full."
  (= (mod (aref deque 1) (length (aref deque 3)))
     (mod (1+ (aref deque 2)) (length (aref deque 3)))))

(defun pq-deque-empty-p (deque)
  "Return non nil if DEQUE is not empty."
  (= (aref deque 1) (aref deque 2)))

(defun pq-deque-each (deque func)
  "Call FUNC for each element in DEQUE."
  (if (>= (aref deque 2) (aref deque 1))
      (let ((cursor (aref deque 1)))
        (while (< cursor (aref deque 2))
          (funcall func (aref (aref deque 3) cursor))
          (setq cursor (1+ cursor)))
        deque)
    (let ((cursor (aref deque 1)))
      (while (< cursor (length (aref deque 3)))
        (funcall func (aref (aref deque 3) cursor))
        (setq cursor (1+ cursor))))
    (let ((cursor 0))
      (while (< cursor (aref deque 2))
        (funcall func (aref (aref deque 3) cursor))
        (setq cursor (1+ cursor))))
    deque))

(defun pq-deque-each-indexed (deque func)
  "Call FUNC for each element in DEQUE with index as first argument."
  (let ((index 0))
    (pq-deque-each
     deque
     (lambda (elem)
       (funcall func index elem)))
    deque))

(defun pq-deque-grow (deque)
  "Grow size of DEQUE."
  (let ((index 0)
        (new (make-vector (* (length (aref deque 3)) 2) nil)))
    (pq-deque-each
     deque
     (lambda (elem)
       (aset new index elem)
       (setq index (1+ index))))
    (aset deque 1 0)
    (aset deque 2 index)
    (aset deque 3 new)
    deque))

(defun pq-deque-peek-front (deque)
  "Peek value from front of DEQUE without actually removing it."
  (aref (aref deque 3) (aref deque 1)))

(defun pq-deque-push-front (deque value)
  "Insert VALUE into front of DEQUE."
  (when (pq-deque-full-p deque)
    (pq-deque-grow deque))
  (pq-deque-move-head-backward deque)
  (aset (aref deque 3) (aref deque 1) value)
  deque)

(defun pq-deque-pop-front (deque)
  "Get and remove value from front of DEQUE."
  (unless (pq-deque-empty-p deque)
    (prog1 (pq-deque-peek-front deque)
      (pq-deque-move-head-forward deque))))

(defun pq-deque-peek-back (deque)
  "Peek value from back of DEQUE without actually removing it."
  (aref (aref deque 3) (mod (1- (aref deque 2)) (length (aref deque 3)))))

(defun pq-deque-push-back (deque value)
  "Insert VALUE into back of DEQUE."
  (when (pq-deque-full-p deque)
    (pq-deque-grow deque))
  (aset (aref deque 3) (aref deque 2) value)
  (pq-deque-move-tail-forward deque)
  deque)

(defun pq-deque-pop-back (deque)
  "Get and remove value from back of DEQUE."
  (unless (pq-deque-empty-p deque)
    (prog1 (pq-deque-peek-back deque)
      (pq-deque-move-tail-backward deque))))

(defun pq-deque-length (deque)
  "Get number of elements in DEQUE."
  (if (>= (aref deque 2) (aref deque 1))
      (- (aref deque 2) (aref deque 1))
    (+ (aref deque 2) (- (length (aref deque 3)) (aref deque 1)))))

(provide 'pq-deque)
;;; pq-deque.el ends here
