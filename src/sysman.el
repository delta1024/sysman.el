(defvar sysman-config-folder 'nil "The directory which houses your guix channel repo folder")

(defvar sysman-watched-folders '() "The folders you access the most in your guix channel. These are reletive to `sysman-conifg-folder'")

(defvar sysman--buffer-name "*sysman pannel*" "the default sysman buffer name value")

(defvar sysman-repo-folder 'nil "if non-nil this variable is appended to `sysman-config-folder' when `sysman--canonicalize-folder-path' is run")

;; (setq sysman-config-folder "~/.system"
;;       sysman-repo-folder "d1024"
;;       sysman-watched-folders '("d1024" "d1024/services" "d1024/services/emacs"))


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

(defun sysman-format-buffer-hook ()
  "formats the default *sysman pannel*"
  (let ((folders (sysman--get-watched-folders-contents)))
    (with-current-buffer-window (get-buffer-create sysman--buffer-name) sysman--buffer-name nil
      (erase-buffer)
      (if sysman-repo-folder
	  (insert (format "Project Root:     %s\nGuix Repo Folder: %s\n"
			  (expand-file-name sysman-config-folder)
			  (expand-file-name sysman-repo-folder sysman-config-folder)))
	(insert (format "Project Root: %s\n" (expand-file-name sysman-config-folder))))
      
      (dolist (folder-list folders)
	(insert (propertize (format "\n%s:\n" (pop folder-list))  'directory 't))
	(dolist (files folder-list)
	  (insert (propertize (format "\t%s\n" files) 'file 't)))))))
;; (sysman-format-buffer-hook)
(provide 'sysman)

