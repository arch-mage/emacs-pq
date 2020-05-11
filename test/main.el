;;; main --- Main entry for running PQ tests -*- lexical-binding: t -*-

;;; Commentary:

;; To run tests, run this script in batch mode.

;;; Code:

(when noninteractive
  (let* ((load-prefer-newer t)
         (this-file (if load-in-progress load-file-name (buffer-file-name)))
         (this-dir (directory-file-name (file-name-directory this-file)))
         (source-dir (directory-file-name (file-name-directory this-dir))))
    (unless (member source-dir load-path)
      (add-to-list 'load-path source-dir))
    (mapc
     (lambda (file) (load file nil t t nil))
     (directory-files this-dir t "^pq-test"))
    (ert-run-tests-batch-and-exit (and "pq-test-" (pop argv)))))

(provide 'main)
;;; main.el ends here
