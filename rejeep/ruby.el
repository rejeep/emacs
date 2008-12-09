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

             ;; Keybindings to find methods.
             (define-key rinari-minor-mode-map (kbd "C-c c") 'rinari-find-controller)
             (define-key rinari-minor-mode-map (kbd "C-c m") 'rinari-find-model)
             (define-key rinari-minor-mode-map (kbd "C-c v") 'rinari-find-view)
             (define-key rinari-minor-mode-map (kbd "C-c h") 'rinari-find-helper)
             (define-key rinari-minor-mode-map (kbd "C-c g") 'rinari-find-migration)

             ;; Rinaris default are to hard.
             (define-key rinari-minor-mode-map (kbd "C-c C-c c") 'rails-find-controller)
             (define-key rinari-minor-mode-map (kbd "C-c C-c m") 'rails-find-model)
             (define-key rinari-minor-mode-map (kbd "C-c C-c v") 'rails-find-view)
             (define-key rinari-minor-mode-map (kbd "C-c C-c h") 'rails-find-helper)
             (define-key rinari-minor-mode-map (kbd "C-c C-c g") 'rails-find-migration)
             (define-key rinari-minor-mode-map (kbd "C-c C-c l") 'rinari-find-lib)
             (define-key rinari-minor-mode-map (kbd "C-c C-c p") 'rinari-find-public)
             (define-key rinari-minor-mode-map (kbd "C-c C-c j") 'rinari-find-javascript)
             (define-key rinari-minor-mode-map (kbd "C-c C-c s") 'rinari-find-stylesheet)
             (define-key rinari-minor-mode-map (kbd "C-c C-c a") 'rinari-find-file-in-project)
             (define-key rinari-minor-mode-map (kbd "C-c C-c k") 'rinari-find-configuration)
             ))

(add-hook 'rhtml-mode-hook
          '(lambda()
             ;; < should insert tag.
             (setq wrap-region-tag-active t)
             (wrap-region-mode t)

             (defun make-partial(beg end)
               "Create a new partial and yanks the selected region in to it."
               (interactive "r")
               (if (region-selected)
                   (let ((partial-name) (partial) (prev-buffer (buffer-name)))
                     (setq partial-name (read-string "Name of partial? "))
                     (setq partial (concat "_" partial-name ".html.erb"))
                     (cond ((not (file-exists-p partial))
                            (append-to-buffer partial (region-beginning) (region-end))
                            (switch-to-buffer partial)
                            (create-file-buffer partial)
                            (write-file partial)
                            (indent-buffer)
                            (save-buffer)
                            (switch-to-buffer prev-buffer)
                            (insert (concat "<%= render :partial => \"" partial-name "\" %>"))
                            (indent-for-tab-command))
                           (t (message "Partial with that name exists!"))))))

             (define-key rhtml-mode-map (kbd "C-c p") 'make-partial)
             ))

(provide 'ruby)