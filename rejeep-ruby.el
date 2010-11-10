;;; rejeep-ruby.el --- Ruby specific settings



;; Don't mess with my bindings.
(add-hook 'ruby-mode-hook
          (lambda()
            (define-key ruby-mode-map (kbd "C-M-n") 'scroll-up-five)
            (define-key ruby-mode-map (kbd "C-M-p") 'scroll-down-five)))

(dolist (regex '("\\.rake$" "\\.gemspec$" "\\.ru$" "Rakefile$" "Gemfile$" "Capfile$"))
  (add-to-list 'auto-mode-alist `(,regex . ruby-mode)))


(provide 'rejeep-ruby)
