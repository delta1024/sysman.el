;; A Major Mode for writing novels.
(defvar blueprint-project-folder 'nil
  "The folder to scan to build the blueprint should be set a a
  dir-local-variable")
;; (setq blueprint-project-folder (expand-file-name "blueprint.el" "~/Projects/Code"))

(defvar blueprint-buffer-name "*blueprint pannel*"
  "the buffer name that is prefixed to the projecto folder name")
(defvar blueprint-output-buffer "*blueprint output*"
  "name prepended to shell command output buffers")
(defvar blueprint--default-ls-command "ls -R"
  "the default ls command run by `blueprint--format-ls'")

(defun blueprint--format-ls ()
  "formats the output of `ls -r' to a syntax `blueprint-mode' can understand"
  (let ((temp-buff (get-buffer-create
		    (concat blueprint-output-buffer ":" blueprint--default-ls-command)))
	(top-level-syntax "^\.:$")
	(file-level-syntax "^\./.*"))
    (with-current-buffer temp-buff
      (erase-buffer)
      (shell-command blueprint--default-ls-command temp-buff))
    (with-current-buffer temp-buff
      (goto-char (point-min))
      (require 'loop)
      (loop-while (/= (point-max) (point))
	(pcase (thing-at-point 'line t);; get line  (looking-at top-level-syntax)
	  ;; Matches top level heading
	  ((pred (lambda (string)
		   (string-match-p top-level-syntax string)))
	   (funcall (lambda ()
		      (delete-region (line-beginning-position) (line-end-position))
		      (insert "Project Root:")
		      nil)))

	  ;; Matches folder headings
	  ((pred (lambda (string)
		   (string-match-p file-level-syntax string)))
	   (funcall (lambda ()
		      (let ((line-string (buffer-substring (line-beginning-position) (line-end-position))))
			(delete-region (line-beginning-position) (line-end-position))
			(insert (substring line-string 2)))
		      nil)))

	  ;; Matches everything else
	  (_
	   (funcall (lambda ()
		      (beginning-of-line)
		      (insert "    ")))))
	(next-line)
	))
      temp-buff))
	     
(defun blueprint--format-buffer (buffer project-root)
  "Assumes that BUFFER is the current `blueprint-buffer-name'
that DEFAULT-DIR is `blueprint-project-folder' or the directory
you wish to be treted as the project root, and that
`buffer-read-only' is set to `nil'."
  (cd project-root)
  (insert (format "Project Root: %s\n\n" project-root))
  (insert-buffer (blueprint--format-ls)))


(defun blueprint--revert-buffer (ignore-auto noconfirm)
  "reverts the buffer by calling `blueprint---format-buffer'
after setting `buffer-read-only' to `nil'"
  (with-current-buffer (get-buffer blueprint-buffer-name)
    (setq-local buffer-read-only 'nil)
    (blueprint--format-buffer (current-buffer) blueprint-project-folder)
    (setq-local buffer-read-only t)))

(defun blueprint-revert-hook ()
  (blueprint--revert-buffer nil nil))

(add-hook 'blueprint-mode-hook 'blueprint-revert-hook)

(defun blueprint ()
  "Sets the environment variables for for the proper functioning of `blueprint-mode'"
  (interactive)
  (let* ((blueprint-project-folder (if blueprint-project-folder
				       blueprint-project-folder
				     (call-interactively (lambda (folder)
							   (interactive "GProject Root: ")
							   folder))))
	 (blueprint-buffer-name (concat blueprint-buffer-name ":" blueprint-project-folder))
	 (blueprint-active-buffer (get-buffer-create blueprint-buffer-name)))
    (with-current-buffer-window blueprint-active-buffer blueprint-active-buffer nil
      (blueprint-mode))))

(define-derived-mode blueprint-mode special-mode "Blueprint" "Major mode for managing novel files"
  (setq-local revert-buffer-function #'blueprint--revert-buffer))

(provide 'blueprint)
