;;; pq --- PostgreSQL client -*- lexical-binding: t -*-

;;; Commentary:

;; An asynchronous PostgreSQL interface.

;;; Code:

(require 'seq)
(require 'sasl)
(require 'pq-bindat)
(require 'pq-packet)
(require 'pq-error)
(require 'pq-future)

(defun pq--add-listener (proc listener)
  (push listener (process-get proc 'listeners)))

(defun pq--del-listener (proc listener)
  (process-put
   proc
   'listeners
   (seq-remove (apply-partially 'eq listener) (process-get proc 'listeners))))

(defun pq--resignal (err)
  (cond
   ((and (listp err) (symbolp (car err)) (get (car err) 'error-conditions))
    (signal (car err) (cdr err)))
   ((stringp err) (error "%s" err))
   (t (error "%S" err))))

(defun pq--process-filter (proc data)
  (setq data (concat (process-get proc 'buffer) data))

  (catch 'break
    (while (>= (length data) 5)
      (seq-let [_ size] (pq-bindat-unpack '[u8 u32] (substring data 0 5))
        (unless (>= (length data) (1+ size))
          (throw 'break nil))

        (let ((chunk (substring data 0 (1+ size))))
          (mapc
           (lambda (func)
             (condition-case err (funcall func chunk)
               (error (message "Error on listener: %s" err))))
           (process-get proc 'listeners)))

        (setq data (substring data (1+ size))))))

  (process-put proc 'buffer data))

(defun pq--process-sentinel (_proc _msg)
  )

(defun pq--enqueue-task (proc func &rest args)
  (let ((current (process-get proc 'task)))
    (if (and current (pq-future-pending-p current))
        (process-put proc 'task (pq-future:then current (_) (apply func args)))
      (if (pq-future-rejected-p current)
          (pq--resignal (pq-future-current-value current))
        (process-put proc 'task (apply func args))))
    (process-get proc 'task)))

(defun pq--startup-task (proc database user password)
  (pq-future (resolve reject)
    (let ((startup-params (list :user user
                                :client_encoding "UTF8"
                                :application_name "emacs"))
          listener sasl-client sasl-step)

      (when database
        (setq startup-params (plist-put startup-params "database" database)))

      (setq
       listener
       (lambda (data)
         (condition-case err
             (seq-let (code &rest data) (pq--decode-packet data)
               (cond
                ((= code ?K))
                ((= code ?N)
                 (message "%s: %s" (alist-get 'S data) (alist-get 'M data)))
                ((= code ?S))
                ((= code ?R)
                 (seq-let (auth &rest data) data
                   (cond
                    ((= auth 0))
                    ((= auth 10)
                     (let* ((sasl-mechanisms '("SCRAM-SHA-256"))
                            (sasl-mechanism-alist '(("SCRAM-SHA-256" pq-sasl-scram-sha-256)))
                            (mechanism (sasl-find-mechanism data)))
                       (unless mechanism
                         (signal 'pq-error-sasl-mechanisms-not-supported data))
                       (setq sasl-client (sasl-make-client mechanism user nil nil))
                       (setq sasl-step (sasl-next-step sasl-client sasl-step))
                       (process-send-string
                        proc
                        (pq--encode-packet-sasl-response
                         (sasl-step-data sasl-step)
                         (sasl-mechanism-name mechanism)))))
                    ((= auth 11)
                     (let ((sasl-read-passphrase (lambda (_prompt) password)))
                       (sasl-step-set-data sasl-step data)
                       (setq sasl-step (sasl-next-step sasl-client sasl-step))
                       (process-send-string
                        proc
                        (pq--encode-packet-sasl-response
                         (sasl-step-data sasl-step)))))
                    ((= auth 12)
                     (sasl-step-set-data sasl-step data)
                     (setq sasl-step (sasl-next-step sasl-client sasl-step)))
                    (t
                     (signal 'pq-error-unexpected-auth-response auth)))))
                ((= code ?Z)
                 (pq--del-listener proc listener)
                 (funcall resolve proc))
                ((= code ?E)
                 (signal 'pq-error-response data))
                (t (signal 'pq-error-unexpected-message-type code))))
           (error
            (pq--del-listener proc listener)
            (funcall reject err)))))
      (pq--add-listener proc listener)

      (condition-case err
          (process-send-string proc (pq--encode-packet-startup startup-params))
        (error (funcall reject err))))))

(defun pq--startup (proc database user password)
  (pq--enqueue-task proc 'pq--startup-task proc database user password))

(defun pq--query-task (proc params init on-data)
  (pq-future (resolve reject)
    (let ((row-num 0) (accum init) listener columns)
      (setq
       listener
       (lambda (data)
         (condition-case err
             (seq-let (code &rest data) (pq--decode-packet data)
               (cond
                ((= code ?1)) ;; parse complete
                ((= code ?2)) ;; bind complete
                ((= code ?3)) ;; close complete
                ((= code ?C)) ;; command complete
                ((= code ?n)) ;; no data
                ((= code ?N)
                 (message "%s: %s" (alist-get 'S data) (alist-get 'M data)))
                ((= code ?Z)
                 (pq--del-listener proc listener)
                 (funcall resolve accum))
                ((= code ?E)
                 (signal 'pq-error-response data))
                ((= code ?T)
                 (setq columns data))
                ((= code ?D)
                 (when on-data
                   (pcase (cdr (func-arity on-data))
                     (1 (setq accum (funcall on-data data)))
                     (2 (setq accum (funcall on-data data columns)))
                     (3 (setq accum (funcall on-data data columns row-num)))
                     (4 (setq accum (funcall on-data data columns row-num accum)))
                     (_ (signal 'wrong-number-of-arguments (list on-data 1)))))
                 (setq row-num (1+ row-num)))
                (t
                 (signal 'pq-error-unexpected-message-type code))))
           (error
            (pq--del-listener proc listener)
            (funcall reject err)))))

      (pq--add-listener proc listener)
      (condition-case err
          (process-send-string
           proc
           (concat (pq--encode-packet-parse (if (stringp params) params (car params)))
                   (pq--encode-packet-bind (when (listp params) (cdr params)))
                   (pq--encode-packet-describe ?P)
                   (pq--encode-packet-execute)
                   ;; (pq--encode-packet-close ?S)
                   (pq--encode-packet-sync)))
        (error
         (pq--del-listener proc listener)
         (funcall reject err))))))

(defun pq--query (proc params init on-data)
  (pq--enqueue-task proc 'pq--query-task proc params init on-data))

(defun pq-connect (&optional options)
  "Connect to PostgreSQL server using OPTIONS plist.

Recognized options are: :host, :port, :user, :database, :password and
:proc-name.

If :host is prefixed with \"/\", then it's treated as a path to socket
and :port will be ignored.

:proc-name is the process name, so you can get the process object in
other place with `get-process'."
  (let ((user (or (plist-get options :user) user-login-name))
        (host (or (plist-get options :host) "127.0.0.1"))
        (port (or (plist-get options :port) 5432))
        (name (or (plist-get options :proc-name) "pq"))
        (database (plist-get options :database))
        (password (plist-get options :password))
        (proc-params (list :coding 'binary))
        proc)

    (setq proc-params (plist-put proc-params :name name))

    (if (string-prefix-p "/" host)
        (progn
          (setq proc-params (plist-put proc-params :family 'local))
          (setq proc-params (plist-put proc-params :remote host)))
      (setq proc-params (plist-put proc-params :family 'ipv4))
      (setq proc-params (plist-put proc-params :host host))
      (setq proc-params (plist-put proc-params :service port)))

    (setq proc (apply 'make-network-process proc-params))

    (set-process-filter proc 'pq--process-filter)
    (set-process-sentinel proc 'pq--process-sentinel)
    (pq--startup proc database user password)))

(defmacro pq:then (arglist &rest body)
  "Enqueue future task.

Evaluate BODY with VAL bound to result of previous task.

\(fn (VAL) BODY...)"
  (declare (indent 1))
  (let ((future (gensym "future-")))
    `(lambda (,future)
       (pq-future-then
        ,future
        (lambda (,@arglist) ,@body)))))

(defmacro pq:catch (arglist &rest body)
  "Handle an error occurred in future task chain.

Evaluate BODY with ERR bound to error of previous task.

\(fn (ERR) BODY...)"
  (declare (indent 1))
  (let ((future (gensym "future-")))
    `(lambda (,future)
       (pq-future-catch
        ,future
        (lambda (,@arglist) ,@body)))))

(defmacro pq:chain (future &rest chains)
  "Execute CHAINS of future task starting with FUTURE."
  (declare (indent 1))
  (let ((sym (gensym)))
    `(let* ((,sym ,future)
            (,sym (if (pq-future-p ,sym) ,sym (pq-future-resolve nil))))
       ,@(cl-loop for chain in chains
                  collect
                  `(setq ,sym (funcall ,chain ,sym)))
       ,sym)))

(defmacro pq:with (spec &rest chains)
  "Create CHAINS of program in the context of given connection SPEC.

SPEC can be:

A `pq-future' object that will resolve to process object.

A process object that is resolved from `pq-connect'.

A list with the same format as `pq-connect'."
  (declare (indent 1))
  (let ((conn (gensym "conn-"))
        (res  (gensym "res-")))
    `(let ((,conn ,spec))
       (cond
        ((pq-future-p ,conn)
         (pq-future-then
          ,conn
          (lambda (,res)
            (let ((pq--current-process ,res))
              (pq:chain (pq-future-resolve pq--current-process) ,@chains)))))
        ((processp ,conn)
         (let ((pq--current-process ,conn))
           (pq:chain (pq-future-resolve pq--current-process) ,@chains)))
        ((listp ,conn)
         (pq-future-then
          (pq-connect ,conn)
          (lambda (,res)
            (let ((pq--current-process ,res))
              (pq:chain (pq-future-resolve pq--current-process) ,@chains)))))
        (t (signal 'wrong-type-argument ,conn))))))

(defmacro pq:with-temp-conn (options &rest body)
  "Same as `pq-with', but delete the associated process afterward."
  (declare (indent 1))
  (let ((prev (gensym "prev-")))
    `(progn
       (unless (listp ,options)
         (signal 'wrong-type-argument ,options))
       (pq:with ,options
         ,@body
         (pq:then (,prev)
           (when (process-live-p pq--current-process)
             (delete-process pq--current-process))
           ,prev)
         (pq:catch (err)
           (when (process-live-p pq--current-process)
             (delete-process pq--current-process))
           (pq--resignal err))))))

(defmacro pq:query (query arglist &rest body)
  "Evaluate BODY for each row returned from QUERY.

QUERY can be a string or a list in the form of (query params...).

This macro must be executed inside `pq:with' or `pq:with-temp-conn'.

\(fn QUERY (ROW [COL-SPEC IDX RESULT]) BODY...)"
  (declare (indent 2))
  (let ((future  (gensym "future-"))
        (prev    (gensym "prev-")))
    `(lambda (,future)
       (pq-future-then
        ,future
        (lambda (,prev)
          (pq--query
           pq--current-process
           ,query
           ,prev
           (lambda (,@arglist)
             ,@body)))))))

(defmacro pq:exec (query)
  "Just execute query.

This macro must be executed inside `pq:with' or `pq:with-temp-conn'."
  (declare (indent 2))
  (let ((future  (gensym "future-"))
        (prev    (gensym "prev-")))
    `(lambda (,future)
       (pq-future-then
        ,future
        (lambda (,prev)
          (pq--query
           pq--current-process
           ,query
           ,prev
           (lambda (_row _col _idx ,prev) ,prev)))))))

(provide 'pq)
;;; pq.el ends here
