;;; rejeep-rails.el --- Rails specific settings

;; Rinari
(require 'rinari)
(add-hook 'rinari-minor-mode-hook
          (lambda()
             ;; No # -*- coding: utf-8 -*- commets.
             (setq ruby-insert-encoding-magic-comment nil)

             ;; TAGS file is in rails root if any.
             (setq rinari-tags-file-name "TAGS")))

;; Rhtml
(autoload 'rhtml-mode "rhtml-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb$" . rhtml-mode))

(provide 'rejeep-rails)
