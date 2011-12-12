;;; rejeep-javascript.el --- Javascript stuff


(add-hook 'js-mode-hook
          (lambda()
            (setq js-indent-level 2)))

(add-hook 'coffee-mode-hook
          (lambda ()
            (setq coffee-tab-width 2)
            (setq coffee-cleanup-whitespace nil)))


(provide 'rejeep-javascript)
