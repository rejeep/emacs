;;; rejeep-css.el --- CSS specific settings.

(eval-after-load 'css-mode
  '(progn
     (wrap-region-mode t)

     ;; Indention width is two spaces.
     (setq css-indent-offset 2)
     ))

(provide 'rejeep-css)