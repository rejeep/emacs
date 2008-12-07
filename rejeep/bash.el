;;; bash.el --- (ba)sh specific settings.

(add-hook 'sh-mode-hook
          '(lambda()
             (wrap-region-mode t)

             ;; Indent width is two spaces.
             (setq sh-indentation 2)
             ))

(provide 'bash)