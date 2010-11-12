;;; rejeep-programming.el --- Programming stuff


;; Snippets
(require 'dropdown-list)
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (expand-file-name emacs-dir "snippets"))

(setq-default
 yas/prompt-functions
 '(yas/ido-prompt yas/dropdown-prompt yas/x-prompt yas/completing-prompt yas/no-prompt))

(require 'markdown-mode)
(require 'yaml-mode)
(require 'feature-mode)
(require 'html-script-src)
(require 'haml-mode)
(require 'sass-mode)
(require 'rejeep-java)
(require 'rejeep-ruby)
(require 'rejeep-rails)
(require 'rejeep-css)
(require 'rejeep-javascript)
(require 'rejeep-bash)
(require 'rejeep-lisp)
(require 'rejeep-c)
(require 'rejeep-git)
(require 'rejeep-haskell)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
(add-to-list 'auto-mode-alist '("\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\.md$" . markdown-mode))

(add-hook 'compilation-shell-minor-mode-hook 'ansi-color-for-comint-mode-on)


(provide 'rejeep-programming)
