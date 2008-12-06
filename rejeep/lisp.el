;;; lisp.el --- All that has to do with (any dialect of) Lisp

(autoload 'stumpwm-mode "stumpwm-mode" nil t)

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (wrap-region-bind-keys emacs-lisp-mode-map "\"" "'" "(")

             ;; Completion.
             (define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)
             ))

(provide 'lisp)