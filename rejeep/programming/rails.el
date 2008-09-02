;; Ruby on Rails minor mode.
(require 'rinari)

;; For rhtml and html.erb files.
(require 'rhtml-mode)


(add-hook 'rinari-minor-mode-hook
          '(lambda()
             ;; TAGS file is in rails root if any.
             (setq rinari-tags-file-name "TAGS")
             ))

;; Use rhtml-mode for html.erb files.
(add-to-list 'auto-mode-alist '("\\.html.erb$" . rhtml-mode))