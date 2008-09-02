;; Ruby on Rails minor mode.
(require 'rinari)

;; For rhtml and html.erb files.
(require 'rhtml-mode)


(add-hook 'rinari-minor-mode-hook
          '(lambda()
             ;; TAGS file is in rails root if any.
             (setq rinari-tags-file-name "TAGS")

             ;; Keybindings to Rinari find methods.
             (define-key rinari-minor-mode-map (kbd "C-c c") 'rinari-find-controller)
             (define-key rinari-minor-mode-map (kbd "C-c m") 'rinari-find-model)
             (define-key rinari-minor-mode-map (kbd "C-c v") 'rinari-find-view)
             (define-key rinari-minor-mode-map (kbd "C-c h") 'rinari-find-helper)
             (define-key rinari-minor-mode-map (kbd "C-c j") 'rinari-find-javascript)
             (define-key rinari-minor-mode-map (kbd "C-c l") 'rinari-find-lib)
             (define-key rinari-minor-mode-map (kbd "C-c g") 'rinari-find-migration)
             (define-key rinari-minor-mode-map (kbd "C-c p") 'rinari-find-public)
             (define-key rinari-minor-mode-map (kbd "C-c s") 'rinari-find-stylesheet)
             (define-key rinari-minor-mode-map (kbd "C-c f") 'rinari-find-file-in-project)
             (define-key rinari-minor-mode-map (kbd "C-c k") 'rinari-find-configuration)
             ))


;; Use rhtml-mode for html.erb files.
(add-to-list 'auto-mode-alist '("\\.html.erb$" . rhtml-mode))