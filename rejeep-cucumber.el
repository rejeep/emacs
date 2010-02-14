;;; rejeep-cucumber.el --- Cucumber settings

(add-to-list 'load-path "~/.emacs.d/packages/cucumber")
(require 'feature-mode)

(add-hook 'feature-mode-hook
          (lambda()
            (outline-minor-mode 1)
            (setq outline-regexp " *\\(Feature:\\|Scenario:\\|Background:\\)")
            (define-key feature-mode-map (kbd "C-c <tab>") 'outline-toggle-children)))

(provide 'rejeep-cucumber)
