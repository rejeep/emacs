;; Use yaml-mode for yml files.
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; No # -*- coding: utf-8 -*- commets.
(setq ruby-insert-encoding-magic-comment nil)