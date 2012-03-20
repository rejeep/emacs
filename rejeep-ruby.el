;;; rejeep-ruby.el --- Ruby specific settings

(require 'rvm)
(require 'ruby-end)

(setq ruby-end-check-statement-modifiers t)

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))
(ad-activate 'rspec-compile)

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

(add-hook 'ruby-mode-hook 'flymake-ruby-load)

(dolist (regex '("\\.rake$" "\\.gemspec$" "\\.ru$" "Rakefile$" "Gemfile$" "Capfile$"))
  (add-to-list 'auto-mode-alist `(,regex . ruby-mode)))

(provide 'rejeep-ruby)
