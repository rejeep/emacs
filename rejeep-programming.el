;;; rejeep-programming.el --- Programming stuff, not connected to any specific language.

(require 'flymake)

(add-hook 'c-mode-common-hook (lambda () (c-subword-mode 1)))

;; Markdown
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-to-list 'auto-mode-alist '("\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\.md$" . markdown-mode))

;; Snippets
(require 'dropdown-list)
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

;; Yaml
(require 'yaml-mode)
(add-hook 'yaml-mode-hook 'flyspell-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Curly bracket languages
(add-hook 'wrap-region-after-insert-twice-hook
          (lambda ()
            (let ((modes '(c-mode java-mode javascript-mode css-mode)))
              (if (and (string= (char-to-string (char-before)) "{") (member major-mode modes))
                  (let ((origin (line-beginning-position)))
                    (newline 2)
                    (indent-region origin (line-end-position))
                    (forward-line -1)
                    (indent-according-to-mode))))))

(provide 'rejeep-programming)
