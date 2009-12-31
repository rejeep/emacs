;;; rejeep-programming.el --- Programming stuff, not connected to any specific language.

(add-hook 'c-mode-common-hook
          (lambda () (c-subword-mode 1)))

;; Markdown
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\.markdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          '(lambda()
             (wrap-region-set-mode-punctuations '("[" "("))
             (wrap-region-mode t)
             ))

;; Magit
(add-hook 'magit-mode-hook 'rinari-launch)
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

;; Snippets
(require 'dropdown-list)
(require 'yasnippet)
(yas/load-directory "~/.emacs.d/snippets")
(yas/initialize)

;; Cucumber
(add-to-list 'load-path "~/.emacs.d/packages/cucumber")
(require 'feature-mode)

;; Yaml
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(provide 'rejeep-programming)