(defvar sysman-config-folder 'nil "The directory which houses your guix channel repo folder")

(defvar sysman-watched-folders '() "The folders you access the most in your guix channel. These are reletive to `sysman-conifg-folder'")

(defvar sysman--buffer-name "*sysman pannel*" "the default sysman buffer name value")

(defvar sysman-repo-folder 'nil "if non-nil this variable is appended to `sysman-config-folder' when `sysman--get-repo-folder' is run")

;; (setq sysman-config-folder "~/.system"
;;       sysman-repo-folder "d1024"
;;       sysman-watched-folders '("d1024" "d1024/services" "d1024/services/emacs"))

(defun sysman--get-repo-folder ()
  "if the value of `sysman-repo-folder' is non-nil, returns
        `sysman-repo-folder'\'s value appended to the value of
        `sysman-config-folder' as a reletive system path. Otherwise
        returns `sysman-config-folder'"
  (if sysman-repo-folder
      (format "%s/%s" sysman-config-folder sysman-repo-folder)
    sysman-config-folder))

(defun sysman--canonicalize-folder-path (folder)
  "returns the full system path of FOLDER reletive to the value
        returned by `sysman--get-repo-folder'"
  (let ((path (sysman--get-repo-folder)))
    (expand-file-name folder path)))


(defun sysman--get-folder-key (folder)
  (let ((key (format ":%s" folder)))
    (intern key)))

(defun sysman--add-source-folder-property (file folder)
  "add the FOLDER as the 'source-dir property fo FILE"
  (propertize file 'source-dir folder))

(defun sysman--get-watched-folders-contents ()
  "runs the ls command on the folders specified in
          `sysman-watched-folders' then places there value in an alist
          containg the directory name as the key value and it's contents
          as it's pair."
  (with-temp-buffer
    (let* ((folder-tree '()))
      (dolist (source-folder sysman-watched-folders)
        (push `(,(sysman--get-folder-key source-folder)
                . ,(let* ((temp-list '())
                          (default-directory (sysman--canonicalize-folder-path source-folder)))
                     (shell-command "ls" (current-buffer))
                     (goto-char (point-min))
                     (next-line)
                     (while (/= (point) (point-max))
                       (push (thing-at-point 'line t) temp-list)
                       (next-line))
                     (erase-buffer)
                     temp-list)) folder-tree))
      
      folder-tree)))
(provide 'sysman)
