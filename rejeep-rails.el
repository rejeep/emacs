;;; rejeep-rails.el --- Rails specific settings


(require 'rinari)

(add-hook 'magit-mode-hook 'rinari-launch)
(add-hook 'rinari-minor-mode-hook
          (lambda()
             ;; No # -*- coding: utf-8 -*- commets.
             (setq ruby-insert-encoding-magic-comment nil)))

;; Rhtml
(autoload 'rhtml-mode "rhtml-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb$" . rhtml-mode))


(provide 'rejeep-rails)
