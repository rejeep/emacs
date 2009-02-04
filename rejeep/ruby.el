;;; ruby.el --- Ruby and Rails specific settings

(autoload 'yaml-mode "yaml-mode" "" t)
(autoload 'ruby-mode "ruby-mode" "" t)
(autoload 'rhtml-mode "rhtml-mode" "" t)

(eval-after-load 'ruby-mode
  '(progn
     (require 'rinari)

     ;; Flymake for on the fly error checking.
     (defun flymake-ruby-init()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "ruby" (list "-c" local-file))))

     (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
     (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
     (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     
     ;; Wraps region like this: "#{REGION}"
     (define-key ruby-mode-map (kbd "C-c w") (lambda (beg end)
                                               (interactive "r")
                                               (save-excursion
                                                 (goto-char end)
                                                 (insert "}\"")
                                                 (goto-char beg)
                                                 (insert "\"#{"))))
     ))

(add-hook 'ruby-mode-hook
          '(lambda()
             (wrap-region-mode t)

             (flymake-mode-on)
             ))

(add-hook 'rinari-minor-mode-hook
          '(lambda()
             ;; No # -*- coding: utf-8 -*- commets.
             (setq ruby-insert-encoding-magic-comment nil)

             ;; TAGS file is in rails root if any.
             (setq rinari-tags-file-name "TAGS")

             ;; Show colors in rinari-console.
             (ansi-color-for-comint-mode-on)

             (defun rails-find-model()
               "Find all models in this project."
               (interactive)
               (rails-find "app/models"))

             (defun rails-find-view()
               "Find all views in this project."
               (interactive)
               (rails-find "app/views"))

             (defun rails-find-controller()
               "Find all controllers in this project."
               (interactive)
               (rails-find "app/controllers"))

             (defun rails-find-helper()
               "Find all helpers in this project."
               (interactive)
               (rails-find "app/helpers"))

             (defun rails-find-migration()
               "Find all migrations in this project."
               (interactive)
               (rails-find "db/migrate"))

             (defun rails-find(location)
               "Generic function for finding all files in some folder in this project."
               (ido-find-file-in-dir (concat (rinari-root) "/" location)))

             ;; Find all *.
             (define-key rinari-minor-mode-map (kbd "C-c C-c c") 'rails-find-controller)
             (define-key rinari-minor-mode-map (kbd "C-c C-c m") 'rails-find-model)
             (define-key rinari-minor-mode-map (kbd "C-c C-c v") 'rails-find-view)
             (define-key rinari-minor-mode-map (kbd "C-c C-c h") 'rails-find-helper)
             (define-key rinari-minor-mode-map (kbd "C-c C-c g") 'rails-find-migration)
             ))

(add-hook 'rhtml-mode-hook
          '(lambda()
             ;; < should insert tag.
             (setq wrap-region-tag-active t)
             (wrap-region-mode t)
             ))

(provide 'ruby)