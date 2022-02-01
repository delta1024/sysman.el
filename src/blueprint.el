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
	     (string-match-p folder "project-root:")))
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

(defun blueprint-push-alist (value key alist)
  (setcdr (assq key alist) (push value (cdr (assq key alist)))))

;; (setq test-alist '((:hello "temp")))

;; (blueprint-push-alist "hi" test-symbol test-alist)


(defun blueprint--cleanup-ls-buffer (buffer)
  (let ((file-level-syntax "^    .*"))
    (with-current-buffer buffer
      (goto-char (point-min))
      (next-line 3)
      (setq-local dir-file-alist '((:project-root . (list))))
      (setcdr (assq :project-root dir-file-alist)
	      (push (string-trim (thing-at-point 'line t))
		    (cdr (assq :project-root dir-file-alist)))))
    (next-line)
      (setcdr (assq :project-root dir-file-alist)
	      (push (string-trim (thing-at-point 'line t))
		    (cdr (assq :project-root dir-file-alist))))
    ))


(defun blueprint--format-ls ()
  "formats the output of `ls -r' to a syntax `blueprint-mode' can understand"
  (let ((temp-buff (get-buffer-create
		    (concat blueprint-output-buffer ":" blueprint--default-ls-command)))
	(top-level-syntax "^\.:$")
	(file-level-syntax "^\./.*")
	(format-top-level-syntax (lambda ()
				   (delete-region (line-beginning-position) (line-end-position))
				   (insert "Project Root:")))
	(add-to-file-structure-alist (lambda ()
				       (let ((line-string (buffer-substring (line-beginning-position) (line-end-position))))
					 (blueprint-push-alist line-string current-directory-level file-structure-alist))))
					 
	(format-file-level-syntax (lambda ()
				    (let ((line-string (buffer-substring (line-beginning-position) (line-end-position))))
				      (setq-local current-directory-level (intern (string-trim-right (concat ":" (substring line-string 2)) ":")))
				      (push `(,current-directory-level) file-structure-alist)
				      ))))

    (with-current-buffer temp-buff
      (erase-buffer)
      (shell-command blueprint--default-ls-command (current-buffer))
      (goto-char (point-min))
      (push '(:project-root) file-structure-alist)
      (loop-while (/= (point-max) (point))
	(pcase (thing-at-point 'line t)
	  ;; Matches top level heading
	  ((pred (lambda (string)
		   (string-match-p top-level-syntax string)))

	   (funcall (lambda ()
		      (setq-local current-directory-level :project-root))
		    ))
	  ;; Matches folder headings
	  ((pred (lambda (string)
		   (string-match-p file-level-syntax string)))

	   (funcall format-file-level-syntax))

	  ;; Matches everything else
	  ((pred (lambda (string)
		   (string-match-p "^\n$" string)))
	   nil)
	  (_
	   (funcall (lambda ()
		      (blueprint-push-alist
		       (buffer-substring (line-beginning-position) (line-end-position))
					    current-directory-level file-structure-alist))
		    )))
	  (next-line))
      (erase-buffer)
      (goto-char (point-min))
      (dolist (item (reverse file-structure-alist))
	(insert (format "\n%s:\n" (substring (symbol-name (car item)) 1)))
	(dolist (folder (cdr item))
	  (insert (format "\t%s\n" folder)))))
      
    temp-buff))
	     
(defun blueprint--format-buffer (buffer project-root)
  "Assumes that BUFFER is the current `blueprint-buffer-name'
that DEFAULT-DIR is `blueprint-project-folder' or the directory
you wish to be treted as the project root, and that
`buffer-read-only' is set to `nil'."
  (cd project-root)
  (setq file-structure-alist '())
  (insert (format "Project Root: %s\n\n" project-root))
  (insert-buffer (blueprint--format-ls))
  (blueprint--cleanup-ls-buffer (current-buffer)))


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
