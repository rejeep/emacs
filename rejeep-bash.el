;;; rejeep-bash.el --- (ba)sh specific settings

(add-hook 'sh-mode-hook 'wrap-region-mode)
(add-hook 'sh-mode-hook
          '(lambda()
             (setq sh-indentation 2)))

(provide 'rejeep-bash)
