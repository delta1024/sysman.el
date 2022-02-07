;;; Title:      Sysman - System Manager
;;; Author:     Jacob Stannix
;;; Created:    02.03.2022
;;;
;;; A Emacs Package for managing my GNU/Guix Linux system
;;; Copyright (c) 2022, Jacob Stannix
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


(defvar-local sysman--heading-alist 'nil
  "holds heading start and end positions")

(defcustom sysman-config-folder 'nil
  "The directory which houses your guix channel repo folder"
  :type '(sexp))

(defcustom sysman-watched-folders '()
  "The folders you access the most in your guix channel. These are reletive to `sysman-conifg-folder'"
  :type '(sexp))

(defvar sysman--buffer-name "*sysman pannel*"
  "the default sysman buffer name value")

(defcustom sysman-repo-folder 'nil
  "if non-nil this variable is appended to `sysman-config-folder' when `sysman--canonicalize-folder-path' is run"
  :type '(string))

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



(defun sysman--canonicalize-folder-path (folder)
  "if `sysman-repo-folder' is non-nil, appends it's value to `sysman-config-folder', returns the full system path of FOLDER reletive to that"
  (let ((path (concat sysman-config-folder (when sysman-repo-folder
					     (format "/%s" sysman-repo-folder)))))
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

(defun sysman--heading-to-symbol ()
  (let ((heading-string (thing-at-point 'line t)))
    (intern (format ":%s" (string-trim-right heading-string ":\n")))))

(defun sysman--build-heading-alist ()
  (let ((heading-alist '()))
    (save-excursion
      (goto-char (point-min))
      (text-property-search-forward 'header t)
      (while (/= (point) (point-max))
	(next-line)
	(push (let* ((heading-symbol (sysman--heading-to-symbol))
		     (start-point (lambda ()
				    (next-line)
				    (point)))
		     (end-point (lambda ()
				  (text-property-search-forward 'header t)
				  (point))))
		`(,heading-symbol . (,(funcall start-point) ,(funcall end-point)))) heading-alist)))
    (reverse heading-alist)))

(defun sysman-initial-format-buffer-hook ()
  "formats the initial *sysman pannel*"
  (let ((folders (sysman--get-watched-folders-contents)))
    (save-excursion 
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
			      'system-path parent-dir-path
			      'header t
			      'font-lock-face 'sysman-header-face))
	  (dolist (files folder-list)
	    (if (f-directory-p (expand-file-name files parent-dir-path))
		(insert (propertize (format "\t%s\n" files)
				    'directory t
				    'parent-dir parent-dir-path
				    'font-lock-face 'sysman-folder-face))
	      (insert (propertize (format "\t%s\n" files)
				  'file t
				  'parent-dir parent-dir-path
				  'font-lock-face 'sysman-file-face))))))
      (setq-local buffer-read-only 't)
      (setq sysman--heading-alist (sysman--build-heading-alist)))))

(defun sysman--show-heading ()
  "shows the subcontents of heading at point"
  (let ((range (alist-get (sysman--heading-to-symbol) sysman--heading-alist)))
    (add-text-properties (nth 0 range) (nth 1 range) '(invisible nil))
    (add-text-properties (line-beginning-position) (line-end-position) '(is-hidden nil))))

(defun sysman--hide-heading ()
  "hides the subcontents of heading at point"
  (let ((range (alist-get (sysman--heading-to-symbol) sysman--heading-alist)))
    (add-text-properties (nth 0 range) (nth 1 range) '(invisible t))
    (add-text-properties (line-beginning-position) (line-end-position) '(is-hidden t))))


(defun sysman-toggle-heading ()
  "Toggles visibility of the curren heading at point"
  (interactive)
  (when (get-text-property (point) 'header)
    (setq-local buffer-read-only 'nil)
    (if (get-text-property (point) 'is-hidden)
	(sysman--show-heading)
      (sysman--hide-heading))
    (setq-local buffer-read-only 't)))

(add-hook 'sysman-mode-hook #'sysman-initial-format-buffer-hook)

(defvar sysman-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "TAB") #'sysman-toggle-heading)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    map))

(defun sysman ()
  "Command to initialize `sysman-mode'. Should be used instead of calling `sysman-mode' directaly"
  (interactive)
  (let ((buffer (get-buffer-create sysman--buffer-name)))
    (with-current-buffer-window buffer buffer nil
      (sysman-mode))))


(define-derived-mode sysman-mode special-mode "Sysman" "Major Mode for managing my GNU/Guix System")

(provide 'sysman)

;; Local Variables:
;; before-save-hook: (lambda nil (indent-region (point-min) (point-max)))
;; sysman-config-folder: "~/.system"
;; sysman-repo-folder: "d1024"
;; sysman-watch-folders: '("d1024" "d1024/services" "d1024/services/emacs")
;; End:
