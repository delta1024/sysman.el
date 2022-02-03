(require 'sys-manager)
(setq sysman-project-folder (expand-file-name "example" user-emacs-directory))
(global-set-key (kbd "C-x C-j") #'sysman)
