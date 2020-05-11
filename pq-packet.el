;;; pq-packet --- Packet encoder and decoder -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'seq)
(require 'pq-error)
(require 'pq-bindat)

(defun pq--decode-packet (raw-packet)
  "Decode RAW-PACKET into Lisp object."
  (let* ((header (pq-bindat-unpack '((code u8) (size u32)) raw-packet))
         (code (alist-get 'code header))
         (packet (substring raw-packet 5)))
    (cons
     code
     (cond
      ((= code ?1) nil)
      ((= code ?2) nil)
      ((= code ?3) nil)
      ((= code ?C) (pq-bindat-unpack '[cstr] packet))
      ((= code ?D)
       (let* ((count (pq-bindat--unpack-u16 `[,packet 0]))
              (columns (make-vector count nil)))
         (setq packet (substring packet 2))
         (dotimes (i count)
           (let ((size (pq-bindat--unpack-i32 `[,packet 0])))
             (setq packet (substring packet 4))
             (when (>= size 0)
               (aset columns i (substring packet 0 size))
               (setq packet (substring packet size)))))
         columns))
      ((= code ?E)
       (mapcar
        (lambda (field) (cons (intern (string (aref field 0))) (substring field 1)))
        (seq-take-while
         (lambda (field) (> (length field) 0))
         (split-string packet "\x00"))))
      ((= code ?K)
       (pq-bindat-unpack '[u32 u32] packet))
      ((= code ?n))
      ((= code ?N)
       (mapcar
        (lambda (field) (cons (intern (string (aref field 0))) (substring field 1)))
        (seq-take-while
         (lambda (field) (> (length field) 0))
         (split-string packet "\x00"))))
      ((= code ?R)
       (let ((auth (pq-bindat--unpack-u32 `[,packet 0])))
         (setq packet (substring packet 4))
         (cons
          auth
          (cond
           ((= auth 0) nil)
           ((= auth 10)
            (seq-take-while
             (lambda (mech) (> (length mech) 0))
             (split-string packet "\x00")))
           (t packet)))))
      ((= code ?T)
       (alist-get
        'columns
        (pq-bindat-unpack
         '((count u16)
           (columns vec (count) [cstr i32 i16 i32 i16 i32 i16]))
         packet)))
      ((= code ?S)
       (pq-bindat-unpack '[cstr cstr] packet))
      ((= code ?Z)
       (aref packet 0))
      (t (signal 'pq-error-unexpected-message-type code))))))


(defun pq--encode-packet (code spec data)
  "Encode packet in DATA according to SPEC.

CODE should be nil for StartUp packet."
  (if (and (null spec) (null data)) (pq-bindat-pack '[u8 u32] `[,code 4])
    (let* ((body (pq-bindat-pack spec data))
           (size (string-bytes body))
           (spec `((size u32)
                   (body bytes ,size)))
           (data `((size . ,(+ size 4))
                   (body . ,body))))
      (when code
        (push '(code u8) spec)
        (push `(code . ,code) data))

      (pq-bindat-pack spec data))))

(defun pq--encode-params (params)
  (mapcar
   (lambda (param)
     (cond
      ((memq param '(t true :true)) "true")
      ((memq param '(f false :false)) "false")
      ((stringp param) param)
      ((numberp param) (number-to-string param))
      ((symbolp param) (symbol-name param))
      ((null param) nil)
      (t (signal 'pq-error-parameter-type param))))
   params))

(defun pq--encode-packet-startup (params)

  (setq params
        (mapcar
         (lambda (param)
           (cond
            ((keywordp param) (substring (symbol-name param) 1))
            ((symbolp param) (symbol-name param))
            ((numberp param) (number-to-string param))
            ((stringp param) param)))
         params))

  (pq--encode-packet
   nil
   (vconcat [u32] (make-vector (length params) 'cstr) [u8])
   (vconcat [#x00030000] params [0])))

(defun pq--encode-packet-sasl-response (data &optional mech)
  (if (null mech)
      (pq--encode-packet
       ?p
       `((data bytes ,(length data)))
       `((data . ,data)))
    (pq--encode-packet
     ?p
     `((mech cstr) (size u32) (data bytes ,(length data)))
     `((mech . ,mech)
       (size . ,(length data))
       (data . ,data)))))

(defun pq--encode-packet-parse (query &optional name)
  (pq--encode-packet ?P '[cstr cstr u16] `[,(or name "") ,query 0]))

(defun pq--encode-packet-bind (params &optional stmt portal)
  (let ((params (pq--encode-params params))
        (stmt (or stmt ""))
        (portal (or portal ""))
        specs binds)

    (setq specs
          (vconcat
           (mapcar
            (lambda (param)
              (if (null param) '((size u32))
                `((size u32)
                  (data bytes ,(length param)))))
            params)))

    (setq binds
          (vconcat
           (mapcar
            (lambda (param)
              (if (null param) '((size . -1))
                `((size . ,(string-bytes param))
                  (data . ,param))))
            params)))

    (pq--encode-packet
     ?B
     `((portal   cstr)
       (stmt     cstr)
       (p-format u16)
       (p-count  u16)
       (params   ,specs)
       (r-format u16))
     `((portal   . ,portal)
       (stmt     . ,stmt)
       (p-format . 0)
       (p-count  . ,(length params))
       (params   . ,binds)
       (r-format . 0)))))

(defun pq--encode-packet-describe (kind &optional name)
  (pq--encode-packet
   ?D
   '((kind u8) (name cstr))
   `((kind . ,kind) (name . ,(or name "")))))

(defun pq--encode-packet-execute (&optional name max)
  (pq--encode-packet
   ?E
   '((name cstr)
     (max  u32))
   `((name . ,(or name ""))
     (max  . ,(or max 0)))))

(defun pq--encode-packet-close (kind &optional name)
  (pq--encode-packet
   ?C
   '((kind u8) (name cstr))
   `((kind . ,kind) (name . ,(or name "")))))

(defun pq--encode-packet-sync ()
  (pq--encode-packet ?S nil nil))

(provide 'pq-packet)
;;; pq-packet.el ends here
