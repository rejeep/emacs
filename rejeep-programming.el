;;; rejeep-programming.el --- Programming stuff


;; Snippets
(require 'dropdown-list)
(require 'yasnippet)
(yas/initialize)
(let ((snippets-dir (expand-file-name "snippets" emacs-dir)))
  (yas/load-directory snippets-dir)
  (setq yas/snippet-dirs snippets-dir))

(setq-default
 yas/prompt-functions
 '(yas/ido-prompt yas/dropdown-prompt yas/x-prompt yas/completing-prompt yas/no-prompt))

(add-hook 'markdown-mode-hook
          (lambda ()
            (define-key markdown-mode-map (kbd "M-n") nil)
            (define-key markdown-mode-map (kbd "M-p") nil)))

(subword-mode 1)

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
;; Use `js-mode' for ActionScript.
(add-to-list 'auto-mode-alist '("\\.as$" . js-mode))

(add-hook 'compilation-shell-minor-mode-hook 'ansi-color-for-comint-mode-on)


(provide 'rejeep-programming)
