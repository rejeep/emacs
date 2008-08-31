(add-hook 'java-mode-hook
          '(lambda()
             ;; Two spaces wide indention.
             (setq c-basic-offset 2)
             ))

(add-hook 'jde-mode-hook
          '(lambda()

             ))