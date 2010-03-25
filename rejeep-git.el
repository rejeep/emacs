;;; rejeep-git.el --- Git from Emacs

(require 'magit)

;; Correct spelling on commit messages.
(add-hook 'magit-log-edit-mode-hook 'flyspell-mode)

;; These are the directories in which I want Magit to look for Git repos.
(setq magit-repo-dirs '("~/dev"))

;; Only look one level down for repos.
(setq magit-repo-dirs-depth 1)

;; Use Ido for selecting repos.
(setq magit-completing-read 'ido-completing-read)

;; Set diff colors.
(add-hook 'magit-mode-hook
          (lambda()
            (set-face-foreground 'magit-diff-add "green3")
            (set-face-foreground 'magit-diff-del "red3")))

(provide 'rejeep-git)
