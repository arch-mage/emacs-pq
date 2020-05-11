;;; pq-bindat --- PQ version of bindat -*- lexical-binding: t -*-

;;; Commentary:

;; This is reimplementation of `bindat' package.
;;
;; The built-in `bindat' package is actually great. But it have some
;; limitation with c style string which makes it cumbersome to do
;; pack/unpack data in PostgreSQL packet.

;;; Code:

(defun pq-u8-to-i8 (num)
  "Convert NUM to 8 bit signed integer."
  (if (< num #x80) num (- num #x100)))

(defun pq-u16-to-i16 (num)
  "Convert NUM to 16 bit signed integer."
  (if (< num #x8000) num (- num #x10000)))

(defun pq-u32-to-i32 (num)
  "Convert NUM to 32 bit signed integer."
  (if (< num #x80000000) num (- num #x100000000)))

(defun pq-bindat--grow-buff (state need)
  "Grow string in STATE if it can't hold length of NEED."
  (unless (>= (- (aref state 2) (aref state 1)) need)
    (setq need (- need (- (aref state 2) (aref state 1))))
    (aset state 0 (concat (aref state 0) (make-string need 0)))
    (aset state 2 (+ need (aref state 2)))))

(defun pq-bindat--pack-u8 (state data)
  "Pack DATA as 8 bits unsigned integer into string in STATE."
  (pq-bindat--grow-buff state 1)
  (aset (aref state 0) (aref state 1) (logand data #xff))
  (aset state 1 (1+ (aref state 1))))

(defun pq-bindat--pack-u16 (state data)
  "Pack DATA as 16 bits unsigned integer into string in STATE."
  (pq-bindat--grow-buff state 2)
  (pq-bindat--pack-u8 state (ash data -8))
  (pq-bindat--pack-u8 state data))

(defun pq-bindat--pack-u32 (state data)
  "Pack DATA as 32 bits unsigned integer into string in STATE."
  (pq-bindat--grow-buff state 4)
  (pq-bindat--pack-u8 state (ash data -24))
  (pq-bindat--pack-u8 state (ash data -16))
  (pq-bindat--pack-u8 state (ash data -8))
  (pq-bindat--pack-u8 state data))

(defun pq-bindat--pack-cstr (state data)
  "Pack DATA as c style string into string in STATE."
  (pq-bindat--grow-buff state (1+ (length data)))
  (dotimes (i (length data))
    (aset (aref state 0) (aref state 1) (aref data i))
    (aset state 1 (1+ (aref state 1))))
  (aset (aref state 0) (aref state 1) 0)
  (aset state 1 (1+ (aref state 1))))

(defun pq-bindat--pack-bytes (state data n)
  "Pack DATA as N bytes into string in STATE."
  (pq-bindat--grow-buff state n)
  (dotimes (i n)
    (aset (aref state 0) (aref state 1) (aref data i))
    (aset state 1 (1+ (aref state 1)))))

(defun pq-bindat--pack-group (state spec data)
  "Pack DATA according to SPEC into string in STATE."
  (while spec
    (let* ((item (if (consp spec) (car spec) spec)))
      (setq spec (when (consp spec) (cdr spec)))

      (pcase item

        ((and (pred consp) field
              (guard (= 2 (length field)))
              (guard (symbolp (car field)))
              (guard (vectorp (cadr field))))
         (pq-bindat--pack-group state (cadr field) (alist-get (car field) data)))

        ((pred vectorp)
         (dotimes (i (length item))
           (pq-bindat--pack-group state (aref item i) (aref data i))))

        ;; packing signed integer is no different with unsigned integer.
        ('u8             (pq-bindat--pack-u8    state data))
        ('u16            (pq-bindat--pack-u16   state data))
        ('u32            (pq-bindat--pack-u32   state data))
        ('i8             (pq-bindat--pack-u8    state data))
        ('i16            (pq-bindat--pack-u16   state data))
        ('i32            (pq-bindat--pack-u32   state data))
        ('cstr           (pq-bindat--pack-cstr  state data))
        (`(bytes (,ref)) (pq-bindat--pack-bytes state data (alist-get ref data)))
        (`(bytes ,n)     (pq-bindat--pack-bytes state data n))

        ;; packing signed integer is no different with unsigned integer.
        (`(,key u8)           (pq-bindat--pack-u8    state (alist-get key data)))
        (`(,key u16)          (pq-bindat--pack-u16   state (alist-get key data)))
        (`(,key u32)          (pq-bindat--pack-u32   state (alist-get key data)))
        (`(,key i8)           (pq-bindat--pack-u8    state (alist-get key data)))
        (`(,key i16)          (pq-bindat--pack-u16   state (alist-get key data)))
        (`(,key i32)          (pq-bindat--pack-u32   state (alist-get key data)))
        (`(,key cstr)         (pq-bindat--pack-cstr  state (alist-get key data)))
        (`(,key bytes (,ref)) (pq-bindat--pack-bytes state (alist-get key data) (alist-get ref data)))
        (`(,key bytes ,n)     (pq-bindat--pack-bytes state (alist-get key data) n))

        (`(,key struct ,spec) (pq-bindat--pack-group state spec (alist-get key data)))
        (`(,key vec (,ref) ,type)
         (let ((len (alist-get ref data))
               (data (alist-get key data)))
           (dotimes (i len)
             (pq-bindat--pack-group state type (aref data i)))))
        (`(,key vec ,len ,type)
         (let ((data (alist-get key data)))
           (dotimes (i len)
             (pq-bindat--pack-group state type (aref data i)))))
        (_ (error "Unrecognized pack spec: %s" item)))
      )))

(defun pq-bindat-pack (spec data &optional output)
  "Pack DATA according to SPEC.

OUTPUT should be specified to avoid reallocating string."
  (let* ((output (or output (make-string 0 0)))
         (state (vector output 0 (length output))))
    (pq-bindat--pack-group state spec data)
    (aref state 0)))

(defun pq-bindat--unpack-u8 (state)
  "Unpack 8 bits unsigned integer from string in STATE."
  (aset state 1 (1+ (aref state 1)))
  (aref (aref state 0) (1- (aref state 1))))

(defun pq-bindat--unpack-u16 (state)
  "Unpack 16 bits unsigned integer from string in STATE."
  (logior
   (ash (pq-bindat--unpack-u8 state) 8)
   (pq-bindat--unpack-u8 state)))

(defun pq-bindat--unpack-u32 (state)
  "Unpack 32 bits unsigned integer from string in STATE."
  (logior
   (ash (pq-bindat--unpack-u8 state) 24)
   (ash (pq-bindat--unpack-u8 state) 16)
   (ash (pq-bindat--unpack-u8 state) 8)
   (pq-bindat--unpack-u8 state)))

(defun pq-bindat--unpack-i8 (state)
  "Unpack 8 bits signed integer from string in STATE."
  (pq-u8-to-i8 (pq-bindat--unpack-u8 state)))

(defun pq-bindat--unpack-i16 (state)
  "Unpack 16 bits signed integer from string in STATE."
  (pq-u16-to-i16 (pq-bindat--unpack-u16 state)))

(defun pq-bindat--unpack-i32 (state)
  "Unpack 32 bits signed integer from string in STATE."
  (pq-u32-to-i32 (pq-bindat--unpack-u32 state)))

(defun pq-bindat--unpack-cstr (state)
  "Unpack c style string from string in STATE."
  (let ((beg (aref state 1))
        (end (aref state 1)))
    (while (/= (aref (aref state 0) end) 0)
      (setq end (1+ end)))
    (aset state 1 (1+ end))
    (substring (aref state 0) beg end)))

(defun pq-bindat--unpack-bytes (state n)
  "Unpack N bytes of data from string in STATE."
  (let ((beg (aref state 1))
        (end (+ (aref state 1) n)))
    (aset state 1 end)
    (substring (aref state 0) beg end)))

(defun pq-bindat--unpack-group (state spec)
  "Unpack data in STATE according to SPEC."
  (let (result)
    (while spec
      (let* ((item (if (consp spec) (car spec) spec)))
        (setq spec (when (consp spec) (cdr spec)))

        (pcase item
          ((and (pred consp) field
              (guard (= 2 (length field)))
              (guard (symbolp (car field)))
              (guard (vectorp (cadr field))))
           (push (cons (car field) (pq-bindat--unpack-group state (cadr field))) result))
          ((pred vectorp)
           (setq result (make-vector (length item) nil))
           (dotimes (i (length item))
             (aset result i (pq-bindat--unpack-group state (aref item i)))))

          ('u8   (setq result (pq-bindat--unpack-u8   state)))
          ('u16  (setq result (pq-bindat--unpack-u16  state)))
          ('u32  (setq result (pq-bindat--unpack-u32  state)))
          ('i8   (setq result (pq-bindat--unpack-i8   state)))
          ('i16  (setq result (pq-bindat--unpack-i16  state)))
          ('i32  (setq result (pq-bindat--unpack-i32  state)))
          ('cstr (setq result (pq-bindat--unpack-cstr state)))

          (`(,key u8)           (push (cons key (pq-bindat--unpack-u8    state)) result))
          (`(,key u16)          (push (cons key (pq-bindat--unpack-u16   state)) result))
          (`(,key u32)          (push (cons key (pq-bindat--unpack-u32   state)) result))
          (`(,key i8)           (push (cons key (pq-bindat--unpack-i8    state)) result))
          (`(,key i16)          (push (cons key (pq-bindat--unpack-i16   state)) result))
          (`(,key i32)          (push (cons key (pq-bindat--unpack-i32   state)) result))
          (`(,key cstr)         (push (cons key (pq-bindat--unpack-cstr  state)) result))
          (`(,key bytes (,ref)) (push (cons key (pq-bindat--unpack-bytes state (alist-get ref result))) result))
          (`(,key bytes ,n)     (push (cons key (pq-bindat--unpack-bytes state n)) result))

          (`(,key struct ,spec)
           (push (cons key (pq-bindat--unpack-group state spec)) result))
          (`(,key vec (,ref) ,type)
           (let* ((len (alist-get ref result))
                  (data (make-vector len  nil)))
             (dotimes (i len)
               (aset data i (pq-bindat--unpack-group state type)))
             (push (cons key data) result)))
          (`(,key vec ,len ,type)
           (let ((data (make-vector len  nil)))
             (dotimes (i len)
               (aset data i (pq-bindat--unpack-group state type)))
             (push (cons key data) result)))
          (_ (error "Unrecognized unpack spec: %s" item)))
        ))
    (if (consp result) (nreverse result) result)))

(defun pq-bindat-unpack (spec data)
  "Unpack DATA according to SPEC."
  (let ((state (vector data 0)))
    (pq-bindat--unpack-group state spec)))

(provide 'pq-bindat)
;;; pq-bindat.el ends here
