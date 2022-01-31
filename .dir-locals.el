;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode . ((eval . (define-key ctl-x-map
			       (kbd "j")
			       (lambda nil
				 (interactive)
				 (async-shell-command "emacs --with-profile blueprint")))))))
