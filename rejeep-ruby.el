;;; rejeep-ruby.el --- Ruby specific settings


(require 'ruby-end)

(defun ruby-interpolate ()
  "In a double quoted string, interpolate."
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

(add-hook 'ruby-mode-hook
          (lambda()
            (define-key ruby-mode-map (kbd "C-M-n") 'scroll-up-five)
            (define-key ruby-mode-map (kbd "C-M-p") 'scroll-down-five)
            (define-key ruby-mode-map (kbd "#") 'ruby-interpolate)))

(dolist (regex '("\\.rake$" "\\.gemspec$" "\\.ru$" "Rakefile$" "Gemfile$" "Capfile$"))
  (add-to-list 'auto-mode-alist `(,regex . ruby-mode)))


(provide 'rejeep-ruby)
