(add-hook 'sh-mode-hook
          '(lambda()
             ;; TODO:
             ;; * Two spaces as indent width
             
             (wrap-region-bind-keys sh-mode-map "\"" "'" "{" "[" "(")
             ))