;; Keybindings.

;; Go to line.
(global-set-key (kbd "M-g") 'goto-line)

;; Show which left parenthesis a right one belongs to by blinking.
(global-set-key (kbd "C-.") 'blink-matching-open)

;; Expand dabbrevs.
(global-set-key (kbd "M-_") 'dabbrev-expand)

;; Switch to next buffer.
(global-set-key (kbd "<C-tab>") 'next-buffer)

;; Switch to previous buffer.
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-buffer)

;; Copy region to clippboard.
(global-set-key (kbd "C-x M-w") 'clipboard-kill-ring-save)

;; Paste from clippboard.
(global-set-key (kbd "C-x M-v") 'clipboard-yank)

;; Newline and then indent.
(global-set-key (kbd "RET") 'newline-and-indent)

;; Opens line below and goes to that line.
(global-set-key (kbd "<C-return>") 'open-line-below)

;; Opens line above and goes to that line.
(global-set-key (kbd "<C-M-return>") 'open-line-above)

;; Fast access to ~/.emacs.
(global-set-key (kbd "C-x e") (lambda() (interactive) (find-file "~/.emacs")))

;; Moves five rows up.
(global-set-key (kbd "<C-up>") '(lambda() (interactive) (next-line -5)))

;; Moves five rows down.
(global-set-key (kbd "<C-down>") '(lambda() (interactive) (next-line 5)))

;; So that Emacs never is quit by mistake.
(global-set-key (kbd "C-x C-c")
                (lambda()
                  (interactive)
                  (if (y-or-n-p "Quit Emacs? ")
                      (save-buffers-kill-emacs))))