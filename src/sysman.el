(defvar sysman-config-folder 'nil "The directory which houses your guix channel repo folder")

(defvar sysman-watched-folders '() "The folders you access the most in your guix channel. These are reletive to `sysman-conifg-folder'")

(defvar sysman--buffer-name "*sysman pannel*" "the default sysman buffer name value")

(defvar sysman-repo-folder 'nil "if non-nil this variable is appended to `sysman-config-folder' when `sysman--canonicalize-folder-path' is run")

(defface sysman-header-face
  '((((type graphic) (background dark))
     :foreground "darkseagreen2"))
  "Face for sysman header lines"
  :group 'sysman-faces)

(defface sysman-file-face
  '((((type graphic) (background dark))
     :foreground "red"))
  "Face for sysman file dispaly"
  :group 'sysman-faces)

(defface sysman-folder-face
  '((((type graphic) (background dark))
     :foreground "gold"))		
 "Face for sysman folder diplay"
  :group 'sysman-faces)

(setq sysman-config-folder "~/.system"
      sysman-repo-folder "d1024"
      sysman-watched-folders '("d1024" "d1024/services" "d1024/services/emacs"))

(defun sysman--canonicalize-folder-path (folder)
  "if `sysman-repo-folder' is non-nil, appends it's value to `sysman-config-folder', returns the full system path of FOLDER reletive to that"
  (let ((path (concat sysman-config-folder (if sysman-repo-folder
					       (format "/%s" sysman-repo-folder) ""))))
    (expand-file-name folder path)))

(defun sysman--get-watched-folders-contents ()
  "creates a list by runing `directory-files' on each folder in
`sysman-watched-folders', removes \".\" and \"..\" and pushes
the parent directory on to the list obtained from
`directory-files', and pushes that value onto the function
return value."
  (let ((folder-structure-list '()))
    (dolist (folder sysman-watched-folders)
      (push (let ((user-folder (directory-files (sysman--canonicalize-folder-path folder))))
	      (pop user-folder)
	      (pop user-folder)
	      (push folder user-folder)
	      user-folder) folder-structure-list))
    (reverse folder-structure-list)))

(defun sysman--format-buffer-function (arg1 arg2)
  "formats the default *sysman pannel*"
  (let ((folders (sysman--get-watched-folders-contents)))
    (with-current-buffer (get-buffer-create sysman--buffer-name)  
      (setq-local buffer-read-only 'nil)
      (erase-buffer)
      (if sysman-repo-folder
	  (insert (format "Project Root:     %s\nGuix Repo Folder: %s\n"
			  (expand-file-name sysman-config-folder)
			  (expand-file-name sysman-repo-folder sysman-config-folder)))
	(insert (format "Project Root: %s\n" (expand-file-name sysman-config-folder))))
      
      (dolist (folder-list folders)
	(let* ((parent-dir (pop folder-list))
	       (parent-dir-path (sysman--canonicalize-folder-path parent-dir)))

	  (insert (propertize (format "\n%s:\n" parent-dir)
			      'file-type 'header
			      'font-lock-face 'sysman-header-face))
	  
	  (dolist (files folder-list)
	    (if (f-directory-p (expand-file-name files parent-dir-path))
		(insert (propertize (format "\t%s\n" files)
				    'file-type 'directory
				    'font-lock-face 'sysman-folder-face))
	      (insert (propertize (format "\t%s\n" files)
				  'file-type 'file
				  'font-lock-face 'sysman-file-face))))))
      (setq-local buffer-read-only 't))))

(defun sysman-init-hook ()
  (sysman--format-buffer-function nil nil))

(add-hook 'sysman-mode-hook #'sysman-init-hook)

(defun sysman ()
  (interactive)
  (with-current-buffer-window
      (get-buffer-create sysman--buffer-name)
      (get-buffer-create sysman--buffer-name)  nil
    (sysman-mode)))

(define-derived-mode sysman-mode special-mode "Sysman" "Major Mode for managing my GNU/Guix system"
  (setq-local revert-buffer-function #'sysman--format-buffer-function))

(provide 'sysman)
