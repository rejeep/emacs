;; Use yaml-mode for yml files.
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))

;; Flymake.
(load "rejeep/programming/flymake/ruby.el")

(add-hook 'ruby-mode-hook
          '(lambda()
             ;; Activate flymake.
             (flymake-mode-on)

             (wrap-region-bind-keys ruby-mode-map "\"" "'" "{" "[" "|" "(")
             
             (require 'rcodetools)
             
             (defun rct-complete-symbol--ido()
               "Method completion using ido-mode"
               (interactive)
               (save-excursion
                 (rct-try-completion))
               (let ((choise (ido-completing-read "> " rct-method-completion-table)))
                 (delete-back-to-period)
                 (insert choise)))
             
             (define-key ruby-mode-map (kbd "M-/") 'rct-complete-symbol--ido)
             ))

(add-hook 'rinari-minor-mode-hook
          '(lambda()
             ;; No # -*- coding: utf-8 -*- commets.
             (setq ruby-insert-encoding-magic-comment nil)
             ))

;; Load Rails configuration file.
(load "rejeep/programming/rails/init.el")