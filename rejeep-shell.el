;;; rejeep-bash.el --- (ba)sh specific settings.

(add-hook 'sh-mode-hook
          '(lambda()
             (wrap-region-mode t)

             ;; Indent width is two spaces.
             (setq sh-indentation 2)

             ;; Flymake for shell mode.
             (require 'flymake-shell)
             (add-hook 'sh-mode-hook 'flymake-shell-load)
             ))

(provide 'rejeep-bash)