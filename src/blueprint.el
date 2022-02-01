;; A Major Mode for writing novels.
(require 'subr-x)
(require 'loop)
(defvar blueprint-project-folder 'nil
  "The folder to scan to build the blueprint pannel.  It is
recomened to set set a dir-local-variable in the project root")

(defvar blueprint-buffer-name "*blueprint pannel*"
  "the buffer name that is prefixed to the project buffer name")
(defvar blueprint-output-buffer "*blueprint output*"
  "name prepended to shell command output buffers")
(defvar blueprint--default-ls-command "ls -R"
  "the default ls command run by `blueprint--format-ls'")

(defun blueprint--get-parent-dir ()
  (let ((inner-level-syntax "^\t.*"))
    (save-excursion
      (beginning-of-line)
      (loop-while (string-match-p inner-level-syntax
				  (buffer-substring (line-beginning-position) (line-end-position)))
	(previous-line))
      (setq-local blueprint--file-parent-folder (string-trim-right
						 (buffer-substring (line-beginning-position)
								   (line-end-position)) ":")))))

(defun blueprint--expand-file-path ()
  (blueprint--get-parent-dir)
  (pcase blueprint--file-parent-folder
    ((pred (lambda (folder)
	     (string-match-p folder "Project Root:")))
     (funcall (lambda ()
		(setq-local blueprint--file-parent-folder blueprint-project-folder))))

    (_
     (funcall (lambda ()
		(setq-local blueprint--file-parent-folder
			    (expand-file-name blueprint--file-parent-folder
					      blueprint-project-folder))))))

  (setq-local blueprint--file-absolute-path
	      (expand-file-name (string-trim
				 (buffer-substring (line-beginning-position) (line-end-position)))
				blueprint--file-parent-folder)))

(defun blueprint-open-file ()
  (interactive)
  (blueprint--expand-file-path)
  (find-file blueprint--file-absolute-path))



	     
(defun blueprint--format-ls ()
  (let* ((top-level-syntax "^\.:$")
	 (dir-level-syntax "^\./.*")
	 (empty-line-syntax "^\n$")
	 (push-file-to-directory (lambda ()
				   "pushes the buffer line at point to the cdr of the alist matching `current-directory-level'"
				   (let ((line-string (buffer-substring (line-beginning-position) (line-end-position))))
				     (setcdr (assq current-directory-level directory-tree-alist)
					     (push line-string (cdr (assq current-directory-level directory-tree-alist))))))) ;; lambda ends 2 parens in
	 
	 (ls-command "ls -R")
	 (working-dir (expand-file-name "example" "~/Projects/Code/blueprint.el")))
    (concat "\n"
	     (with-temp-buffer
	       (cd working-dir)
	       (setq-local directory-tree-alist '())
	       (shell-command ls-command (current-buffer))
	       (goto-char (point-min))
	       (loop-while (/= (point) (point-max))
		 (pcase (thing-at-point 'line t)
		   ((pred (lambda (string)
			    "matches `top-level-syntax'"
			    (string-match-p top-level-syntax string)))
		    (funcall (lambda ()
			       "adds `:root-directory' to
                               `directory-tree-alsit' and 
                                sets `current-level-directory'
                                to the same"
			       (push '(:root-directory) directory-tree-alist)
			       (setq-local current-directory-level :root-directory))))
		   ((pred (lambda (string)
			    "matches `directory-level-syntax'"
			    (string-match-p dir-level-syntax string)))
		    (funcall (lambda ()
			       "grabs the current buffer line as a
			      string and sets it's value to
			      `line-string', it then removes the
			      trailing \":\" and prepends
			      \":\". finally it sets the value of
			      the buffer-local-variable
			      `current-directory-level' to the
			      interned value of `line-string'."
			       (let ((line-string (buffer-substring (line-beginning-position) (line-end-position))))
				 (setq-local current-directory-level (intern (concat ":" (string-trim-right line-string ":"))))
				 (push `(,current-directory-level) directory-tree-alist)))))
		   
		   ((pred (lambda (string)
			    "matches and empty line"
			    (string-match-p empty-line-syntax string)))
		    nil)
		   (_ 				;; Anything else that matches must be a file
		    (funcall push-file-to-directory)))
		 (next-line))
	       
	       (erase-buffer)
	       (insert "Project Root:\n")
	       (setq-local directory-tree-alist (reverse directory-tree-alist))
	       (setq-local root-dir-alist (pop directory-tree-alist))
	       (pop root-dir-alist)
	       (dolist (file-name root-dir-alist)
		 (insert (format "\t%s\n" file-name)))
	       (dolist (folder-name directory-tree-alist)
		 (insert (string-trim-left (format "%s:\n" (car folder-name)) ":\./" ))
		 (dolist (file-name (cdr folder-name))
		   (insert (format "\t%s\n" file-name))))
	       (buffer-string)))))

(defun blueprint--format-buffer (buffer project-root)
  "Assumes that BUFFER is the current `blueprint-buffer-name'
that DEFAULT-DIR is `blueprint-project-folder' or the directory
you wish to be treted as the project root, and that
`buffer-read-only' is set to `nil'."
  (insert (format "Project Root: %s\n\n" project-root))
  (insert (blueprint--format-ls)))


(defun blueprint--revert-buffer (ignore-auto noconfirm)
  "reverts the buffer by calling `blueprint---format-buffer'
after setting `buffer-read-only' to `nil'"
  (with-current-buffer (get-buffer blueprint-buffer-name)
    (setq-local buffer-read-only 'nil)
    (blueprint--format-buffer (current-buffer) blueprint-project-folder)
    (setq-local buffer-read-only t)))

(defun blueprint-revert-hook ()
  (blueprint--revert-buffer nil nil))


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
  (add-hook 'blueprint-mode-hook 'blueprint-revert-hook)
  (define-key blueprint-mode-map (kbd "RET") #'blueprint-open-file)
  (setq font-lock-defaults
	      '(("\\<\\(Project Root:\\)>" 1 font-lock-warning-face)))
  (setq-local revert-buffer-function #'blueprint--revert-buffer))

(provide 'blueprint)
