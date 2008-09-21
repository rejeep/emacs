;; Use yaml-mode for yml files.
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Flymake.
(load "rejeep/programming/flymake/ruby.el")

(add-hook 'ruby-mode-hook
          '(lambda()
             ;; Activate flymake.
             (flymake-mode-on)

             (wrap-region-bind-keys ruby-mode-map "\"" "'" "{" "[" "|")
             ))

(add-hook 'rinari-minor-mode-hook
          '(lambda()
             ;; No # -*- coding: utf-8 -*- commets.
             (setq ruby-insert-encoding-magic-comment nil)
             ))

;; Load Rails configuration file.
(load "rejeep/programming/rails.el")
(global-set-key "'" (wrap-region-with-function "'"))