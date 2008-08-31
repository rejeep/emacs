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

             ;; No "final" when auto creating methods and variables.
             (setq jde-gen-final-arguments nil)
             (setq jde-gen-final-methods nil)

             ;; Don't use JDE's builtin abbrevs.
             (setq jde-enable-abbrev-mode nil)

             ;; So that the left brace is placed on new row.
             (setq jde-gen-k&r nil)
             ))