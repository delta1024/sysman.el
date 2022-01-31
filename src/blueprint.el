;; A Major Mode for writing novels.
(defvar bluprint-user-folder (expand-file-name "example/" user-emacs-directory)
  "The folder to scan to build the blueprint should be set a a
  dir-local-variable")
(defvar blue-print-buffer-name "*blueprint pannel*")
(defun bluprint-mode-init-hook ()
  "Creates the primary blueprint buffer"
  ;; grab a directory tree from `blueprint-user-folder'
  ;; display each file/folder as a line in the buffer
 nil)
(defun blueprint--refresh-function ()
  "refreshes the current buffer"
  nil)
(define-derived-mode blue-print-mode special-mode "Blueprint" "Major mode for managing novel files")
(provide 'blueprint)
