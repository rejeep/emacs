;; Ruby on Rails minor mode.
(require 'rinari)

;; For rhtml and html.erb files.
(require 'rhtml-mode)

(add-hook 'rinari-minor-mode-hook
          '(lambda()
             ;; TAGS file is in rails root if any.
             (setq rinari-tags-file-name "TAGS")

             ;; Rails find helpers.
             (load "rejeep/programming/rails/find.el")

             ;; Keybindings to find methods.
             (define-key rinari-minor-mode-map (kbd "C-c c") 'rinari-find-controller)
             (define-key rinari-minor-mode-map (kbd "C-c m") 'rinari-find-model)
             (define-key rinari-minor-mode-map (kbd "C-c v") 'rinari-find-view)
             (define-key rinari-minor-mode-map (kbd "C-c h") 'rinari-find-helper)
             (define-key rinari-minor-mode-map (kbd "C-c g") 'rinari-find-migration)

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
             (wrap-region-bind-keys rhtml-mode-map "\"" "'")

             (define-key rhtml-mode-map (kbd "<") 'wrap-region-with-tag-or-insert)
             (define-key rhtml-mode-map (kbd "C-c p") 'make-partial)
             
             (defun make-partial(beg end)
               "Puts region in a partial."
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
             ))

;; Use rhtml-mode for html.erb files.
(add-to-list 'auto-mode-alist '("\\.html.erb$" . rhtml-mode))