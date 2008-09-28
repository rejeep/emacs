(require 'css-mode)

(add-hook 'css-mode-hook
  '(lambda()
     (setq css-indent-offset 2)
     
     (wrap-region-bind-keys css-mode-map "\"" "'" "{")
  )
)