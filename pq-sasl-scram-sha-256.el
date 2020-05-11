;;; pq-sasl-scram-sha-256 --- PQ sasl mechanism -*- lexical-binding: t -*-

;;; Commentary:

;; There is a sasl implementation built in emacs. But SCRAM-SHA-256 is
;; not implemented.

;;; Code:

(require 'sasl-scram-rfc)

(defconst pq-sasl-scram-sha-256-steps
  '(sasl-scram-client-first-message
    pq-sasl-scram-sha-256-client-final-message
    pq-sasl-scram-sha-256-authenticate-server))


(defun pq-sha256 (object &optional start end binary)
  "Return the SHA256 (Secure Hash Algorithm) of an OBJECT.
OBJECT is either a string or a buffer.  Optional arguments START and
END are character positions specifying which portion of OBJECT for
computing the hash.  If BINARY is non-nil, return a string in binary
form."
  (secure-hash 'sha256 object start end binary))

(defun pq-sasl-scram-sha-256-client-final-message (client step)
  (sasl-scram--client-final-message
   'pq-sha256 64 32 client step))

(defun pq-sasl-scram-sha-256-authenticate-server (client step)
  (sasl-scram--authenticate-server
   'pq-sha256 64 32 client step))

(put 'pq-sasl-scram-sha-256 'sasl-mechanism
     (sasl-make-mechanism "SCRAM-SHA-256" pq-sasl-scram-sha-256-steps))

(put 'pq-sasl-scram-sha-256 'sasl-mechanism (get 'pq-sasl-scram-sha-256 'sasl-mechanism))

(provide 'pq-sasl-scram-sha-256)
;;; pq-sasl-scram-sha-256.el ends here
