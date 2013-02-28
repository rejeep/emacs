;;; rejeep-git.el --- Git from Emacs

(require 'magit)
(require 'magit-blame)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
(setq magit-set-upstream-on-push t)

(add-hook 'magit-mode-hook
          (lambda ()
            (set-face-foreground 'magit-diff-add "green3")
            (set-face-foreground 'magit-diff-del "red3")))

(setq magit-completing-read-function 'magit-ido-completing-read)

(provide 'rejeep-git)
