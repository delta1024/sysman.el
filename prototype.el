;; A Major Mode for writing novels.
;; Copyright (c) 2022, Jacob Stannix
;; 
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public
;; License along with this program. If not, see
;; https://www.gnu.org/licenses/.

(require 'subr-x)
(require 'loop)

;;;;;;;;;; VARIBALES ;;;;;;;;;;

(defvar sysman-project-folder 'nil
  "The folder to scan to build the sysman pannel.  It is
recomened to set set a dir-local-variable in the project root")

(defvar sysman-buffer-name "*sysman pannel*"
  "the buffer name that is prefixed to the project buffer name")
(defvar sysman-output-buffer "*sysman output*"
  "name prepended to shell command output buffers")
(defvar sysman--default-ls-command "ls -R"
  "the default ls command run by `sysman--format-ls'")

;;;;;;;;;; FACES ;;;;;;;;;;

(defface sysman-heading
  '((((type graphic) (background light))
     :foreground "red"
     :weight bold)
    (((type graphic) (background dark))
     :foreground "green"
     :weight bold)
    )
  "face for headings"
  :group 'sysman-mode)

(defface sysman-root
  '((((type graphic) (background light))
     :foreground "blue"
     :weight bold))
  "face for sysman-root top heading"
  :group 'sysman-mode)

;;;;;;;;;; SYSMAN-OPEN-FILE ;;;;;;;;;;

(defun sysman--get-parent-dir ()
  (let ((inner-level-syntax "^\t.*"))
    (save-excursion
      (beginning-of-line)
      (loop-while (string-match-p inner-level-syntax
				  (buffer-substring (line-beginning-position) (line-end-position)))
	(previous-line))
      (setq-local sysman--file-parent-folder (string-trim-right
					      (buffer-substring (line-beginning-position)
								(line-end-position)) ":")))))

(defun sysman--expand-file-path ()
  (sysman--get-parent-dir)
  (pcase sysman--file-parent-folder
    ((pred (lambda (folder)
	     (string-match-p folder "Project Root:")))
     (funcall (lambda ()
		(setq-local sysman--file-parent-folder sysman-project-folder))))

    (_
     (funcall (lambda ()
		(setq-local sysman--file-parent-folder
			    (expand-file-name sysman--file-parent-folder
					      sysman-project-folder))))))

  (setq-local sysman--file-absolute-path
	      (expand-file-name (string-trim
				 (buffer-substring (line-beginning-position) (line-end-position)))
				sysman--file-parent-folder)))

(defun sysman-open-file ()
  (interactive)
  (if (get-text-property (point) 'sysman-file-level)
      (progn
	(sysman--expand-file-path)
	(find-file sysman--file-absolute-path))
    nil))

;;;;;;;;;; BUFFER FORMATING ;;;;;;;;;;

(defun sysman--format-ls ()
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
	  (_ ;; Anything else that matches must be a file
	   (funcall push-file-to-directory)))
	(next-line))
	   

      (erase-buffer)
      (insert (propertize "Root Dir:\n"
			  'font-lock-face 'sysman-heading ))
      (setq-local directory-tree-alist (reverse directory-tree-alist))
      (setq-local root-dir-alist (pop directory-tree-alist))
      (pop root-dir-alist)
      (dolist (file-name root-dir-alist)
	(insert (propertize (format "\t%s\n" file-name)
			    'sysman-file-level 'file)))
      (dolist (folder-name directory-tree-alist)
	(insert (propertize
		 (string-trim-left (format "%s:\n" (car folder-name)) ":\./" )
		 'font-lock-face 'sysman-heading))
	(dolist (file-name (cdr folder-name))
	  (insert (propertize (format "\t%s\n" file-name)
			      'sysman-file-level 'file))))
      (buffer-string))
    ))

(defun sysman--format-buffer (buffer project-root)
  "Assumes that BUFFER is the current `sysman-buffer-name'
that DEFAULT-DIR is `sysman-project-folder' or the directory
you wish to be treted as the project root, and that
`buffer-read-only' is set to `nil'."
  (insert
   (propertize "Project Root: " 'font-lock-face 'sysman-root))
  (insert (format "%s\n\n" project-root))
  (insert (sysman--format-ls)))


(defun sysman--revert-buffer (ignore-auto noconfirm)
  "reverts the buffer by calling `sysman---format-buffer'
after setting `buffer-read-only' to `nil'"
  (with-current-buffer (get-buffer sysman-buffer-name)
    (setq-local buffer-read-only 'nil)
    (sysman--format-buffer (current-buffer) sysman-project-folder)
    (setq-local buffer-read-only t)))

;;;;;;;;;; HOOKS ;;;;;;;;;;

(defun sysman-revert-hook ()
  (sysman--revert-buffer nil nil))

;;;;;;;;;; MODE ;;;;;;;;;;

(defun sysman ()  "Sets the environment variables for for the proper functioning of `sysman-mode'"
       (interactive)
       (let* ((sysman-project-folder (if sysman-project-folder
					 sysman-project-folder
				       (call-interactively (lambda (folder)
							     (interactive "GProject Root: ")
							     folder))))
	      (sysman-buffer-name (concat sysman-buffer-name ":" sysman-project-folder))
	      (sysman-active-buffer (get-buffer-create sysman-buffer-name)))
    
	 (with-current-buffer-window sysman-active-buffer sysman-active-buffer nil
	   (sysman-mode))))

(define-derived-mode sysman-mode special-mode "Sysman" "Major mode for managing novel files"
  (add-hook 'sysman-mode-hook 'sysman-revert-hook)
  (define-key sysman-mode-map (kbd "RET") #'sysman-open-file)
  (setq-local revert-buffer-function #'sysman--revert-buffer))
