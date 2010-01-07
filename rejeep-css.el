;;; rejeep-css.el --- CSS specific settings.

(add-hook 'css-mode-hook 'wrap-region-mode)

(eval-after-load 'css-mode
  '(progn
     (setq css-indent-level 2)))

(provide 'rejeep-css)
