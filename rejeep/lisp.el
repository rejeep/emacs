;;; lisp.el --- All that has to do with (any dialect of) Lisp

(autoload 'stumpwm-mode "stumpwm-mode" nil t)

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (wrap-region-mode t)

             ;; Completion.
             (define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)

             ;; Replace lambda with lambda.
             (defun pretty-lambdas ()
               (font-lock-add-keywords
                nil `(("(\\(lambda\\>\\)"
                       (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                                 ,(make-char 'greek-iso8859-7 107))
                                 nil))))))

             (pretty-lambdas)
             ))

(provide 'lisp)