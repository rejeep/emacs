;;; rejeep-git.el --- Git from Emacs


(require 'magit)

;; Set diff colors.
(add-hook 'magit-mode-hook
          (lambda()
            (set-face-foreground 'magit-diff-add "green3")
            (set-face-foreground 'magit-diff-del "red3")))


(provide 'rejeep-git)
