(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
             (wrap-region-bind-keys emacs-lisp-mode-map "\"" "'" "(")
             ))