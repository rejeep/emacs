;;; rejeep-ruby.el --- Ruby specific settings

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))

(add-hook 'ruby-mode-hook 'flyspell-prog-mode)
(add-hook 'ruby-mode-hook 'wrap-region-mode)
(add-hook 'ruby-mode-hook
          '(lambda()
             (define-key ruby-mode-map (kbd "C-M-h") 'backward-kill-word)))

(provide 'rejeep-ruby)
