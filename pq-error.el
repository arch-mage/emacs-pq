;;; pq-error --- error data structure -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(define-error 'pq-error
  "PQ error")

(define-error 'pq-error-response
  "PQ response error" 'pq-error)

(define-error 'pq-error-internal
  "PQ internal error" 'pq-error)

(define-error 'pq-error-unexpected-message-type
  "Unexpected message type" 'pq-error-internal)

(define-error 'pq-error-unexpected-auth-response
  "Unexpected authentication response" 'pq-error-internal)

(define-error 'pq-error-sasl-mechanisms-not-supported
  "SASL mechanisms not supported" 'pq-error-internal)

(define-error 'pq-error-user
  "PQ user error")

(define-error 'pq-error-parameter-type
  "Invalid parameter type" 'pq-error-user)

(provide 'pq-error)
;;; pq-error.el ends here
