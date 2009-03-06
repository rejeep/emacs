;;; java.el --- Java specific settings

(add-hook 'jde-mode-hook
          '(lambda()
             (wrap-region-mode t)

             ;; Indent width is two spaces.
             (setq c-basic-offset 2)

             ;; Show warnings and errors in code.
             (require 'jde-eclipse-compiler-server)

             ;; Activate flymake.
             (flymake-mode-on)

             ;; No "final" when auto creating methods and variables.
             (setq jde-gen-final-arguments nil)
             (setq jde-gen-final-methods nil)

             ;; Don't use JDE's builtin abbrevs.
             (setq jde-enable-abbrev-mode nil)

             ;; Place left brace on new row.
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
                   (jde-import-expand-imports)
                   (jde-import-kill-extra-imports)
                   (jde-import-organize)
                   (jde-import-all))))

             (defun jde-complete-ido ()
               "Custom method completion for JDE using ido-mode and yasnippet."
               (interactive)
               (let ((completion-list))
                 (dolist (element (jde-complete-find-completion-for-pair (jde-complete-get-pair (jde-parse-java-variable-at-point) nil) nil))
                   (add-to-list 'completion-list (cdr element)))
                 (if completion-list
                     (let ((choise (ido-completing-read "> " completion-list)) (method))
                       (unless (string-match "^.*()$" choise)
                         (setq method (replace-regexp-in-string ")" "})"(replace-regexp-in-string ", " "}, ${" (replace-regexp-in-string "(" "(${" choise)))))
                       (delete-region (point) (re-search-backward "\\." (line-beginning-position)))
                       (insert ".")
                       (if method
                           (yas/expand-snippet (point) (point) method)
                         (insert choise)))
                   (message "No completions at this point"))))
             ))

(provide 'java)