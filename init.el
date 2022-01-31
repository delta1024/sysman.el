(require 'blueprint)
(setq blueprint-project-folder (expand-file-name "example" user-emacs-directory))
(global-set-key (kbd "C-x C-j") #'blueprint)
