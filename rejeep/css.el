;;; css.el --- CSS specific settings.

(autoload 'css-mode "css-mode" "" t)

(eval-after-load 'css-mode
  '(progn
     (wrap-region-bind-keys css-mode-map "\"" "'" "{")

     ;; Indention width is two spaces.
     (setq css-indent-offset 2)
     ))

(provide 'css)