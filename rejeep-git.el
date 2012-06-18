;;; rejeep-git.el --- Git from Emacs

(require 'magit)
(require 'magit-blame)

(setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
(setq magit-set-upstream-on-push t)

(add-hook 'magit-mode-hook
          (lambda ()
            (set-face-foreground 'magit-diff-add "green3")
            (set-face-foreground 'magit-diff-del "red3")))


(provide 'rejeep-git)
