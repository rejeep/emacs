;;; java.el --- Java specific settings

(add-hook 'jde-mode-hook
          '(lambda()
             (wrap-region-mode t)

             (defun flymake-java-ecj-init ()
               (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                                    'jde-ecj-create-temp-file))
                      (local-file  (file-relative-name
                                    temp-file
                                    (file-name-directory buffer-file-name))))
                 (list "ecj" (list "-Xemacs" "-d" "/dev/null"
                                   "-source" "1.5" "-target" "1.5" "-proceedOnError"
                                   "-classpath"
                                   (jde-build-classpath jde-global-classpath) local-file))))

             (defun flymake-java-ecj-cleanup ()
               "Cleanup after `flymake-java-ecj-init' -- delete temp file and dirs."
               (flymake-safe-delete-file flymake-temp-source-file-name)
               (when flymake-temp-source-file-name
                 (flymake-safe-delete-directory (file-name-directory flymake-temp-source-file-name))))

             (defun jde-ecj-create-temp-file (file-name prefix)
               "Create the file FILE-NAME in a unique directory in the temp directory."
               (file-truename (expand-file-name (file-name-nondirectory file-name)
                                                (expand-file-name  (int-to-string (random)) (flymake-get-temp-dir)))))

             (push '(".+\\.java$" flymake-java-ecj-init flymake-java-ecj-cleanup) flymake-allowed-file-name-masks)
             (push '("\\(.*?\\):\\([0-9]+\\): error: \\(.*?\\)\n" 1 2 nil 2 3 (6 compilation-error-face)) compilation-error-regexp-alist)
             (push '("\\(.*?\\):\\([0-9]+\\): warning: \\(.*?\\)\n" 1 2 nil 1 3 (6 compilation-warning-face)) compilation-error-regexp-alist)

             (c-set-style "bsd")

             ;; Indent width is two spaces.
             (setq c-basic-offset 2)

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

             ;; Run ant task.
             (define-key jde-mode-map (kbd "C-c a") 'jde-ant-build)

             ;; Fix imports.
             (define-key jde-mode-map (kbd "C-c o")
               (lambda ()
                 (interactive)
                 (save-excursion
                   (jde-import-expand-imports)
                   (jde-import-kill-extra-imports)
                   (jde-import-organize)
                   (jde-import-all))))

             (setq jde-space-in-param t)

             (defun jde-complete-ido ()
               "Custom method completion for JDE using ido-mode and yasnippet."
               (interactive)
               (let ((completion-list '()) (variable-at-point (jde-parse-java-variable-at-point)))
                 (dolist (element (jde-complete-find-completion-for-pair variable-at-point nil) nil)
                   (add-to-list 'completion-list (cdr element)))
                 (if completion-list
                     (let ((choise (ido-completing-read "> " completion-list nil nil (car (cdr variable-at-point)))) (method))
                       (unless (string-match "^.*()$" choise)
                         (setq method (replace-regexp-in-string ")" "})" (replace-regexp-in-string ", " "}, ${" (replace-regexp-in-string "(" "(${" choise))))
                         (if jde-space-in-param
                             (setq method (replace-regexp-in-string ")" " )" (replace-regexp-in-string "(" "( " method)))))
                       (delete-region (point) (re-search-backward "\\." (line-beginning-position)))
                       (insert ".")
                       (if method
                           (yas/expand-snippet (point) (point) method)
                         (insert choise)))
                   (message "No completions at this point"))))
             ))

(provide 'java)