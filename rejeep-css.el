;;; rejeep-css.el --- CSS specific settings.

(add-hook 'css-mode-hook
          '(lambda ()
             (wrap-region-mode t)

             (setq css-indent-level 2)))

(provide 'rejeep-css)
