(add-hook 'java-mode-hook
          '(lambda()
             ;; Two spaces wide indention.
             (setq c-basic-offset 2)             
             ))

(add-hook 'jde-mode-hook
          '(lambda()
             (wrap-region-bind-keys jde-mode-map "\"" "'" "{" "[" "|" "(")
             
             ;; Show warnings and errors in code.
             (require 'jde-eclipse-compiler-server)
             
             ;; Activate flymake.
             (flymake-mode-on)
             
             ;; No "final" when auto creating methods and variables.
             (setq jde-gen-final-arguments nil)
             (setq jde-gen-final-methods nil)

             ;; Don't use JDE's builtin abbrevs.
             (setq jde-enable-abbrev-mode nil)

             ;; So that the left brace is placed on new row.
             (setq jde-gen-k&r nil)


             ;; Generate getter and setter methods to variables.
             (define-key jde-mode-map (kbd "C-c C-v w") 'jde-wiz-get-set-methods)
             
             ;; Generate variables and getter and setter methods to them.
             (define-key jde-mode-map (kbd "C-c C-v g") 'jde-gen-get-set-methods)

             ;; Fix imports.
             (define-key jde-mode-map (kbd "C-c o")
               (lambda ()
                 (interactive)
                 (save-excursion
                   (jde-import-all)
                   (jde-import-organize)
                   (jde-import-kill-extra-imports))))
             ))