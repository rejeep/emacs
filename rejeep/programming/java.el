(add-hook 'java-mode-hook
          '(lambda()
             ;; Two spaces wide indention.
             (setq c-basic-offset 2)             
             ))

(add-hook 'jde-mode-hook
          '(lambda()
             ;; Show warnings and errors in code.
             (require 'jde-eclipse-compiler-server)
             
             ;; Turn on flymake-mode.
             (flymake-mode 1)
             ))