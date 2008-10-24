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

;; Activate ECB.
(global-set-key (kbd "<M-f1>") 'ecb-activate)

;; Toggle ECB show and hide.
(global-set-key (kbd "§") 'ecb-toggle-ecb-windows)

;; Mark whole buffer.
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; Indent region or buffer.
(global-set-key (kbd "C-S-f") 'indent-region-or-buffer)

;; Yank.
(global-set-key (kbd "C-v") 'yank)

;; Yank and pop kill ring.
(global-set-key (kbd "M-v") 'yank-pop)

;; Mark word cursor is on.
(global-set-key (kbd "M-a") 'mark-current-word)

;; Back to indentation or beginning of line.
(global-set-key (kbd "<home>") 'back-to-indentation-or-beginning-of-line)

;; Delete work backwards without saving it to the kill ring.
(global-set-key (kbd "<M-backspace>") 'backward-delete-word)

;; Google region.
(global-set-key (kbd "C-c C-c g") 'google-region)

;; Expands a snippet
(global-set-key (kbd "<tab>") 'yas/expand)

;; Comments or uncomments a line or region
(global-set-key (kbd "C-7") 'comment-or-uncomment-whole-lines-region)

;; Moves region to *scratch* buffer.
(global-set-key (kbd "C-c s") 'move-region-to-scratch-buffer)

;; Moves line or region one line down.
(global-set-key (kbd "<C-M-down>") 'drag-line-or-region-down)

;; Moves line or region one line up.
(global-set-key (kbd "<C-M-up>") 'drag-line-or-region-up) 

;; Kills current word or region.
(global-set-key (kbd "C-w") 'kill-region-or-current-word)


;; Move cursor 30 lines down or to end of buffer.
(global-set-key (kbd "<next>") (lambda() (interactive) (forward-line 30)))

;; Move cursor 30 lines up or to beginning of buffer.
(global-set-key (kbd "<prior>") (lambda() (interactive) (forward-line -30)))

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

;; Moves line or region five lines up.
(global-set-key (kbd "<C-M-prior>") '(lambda() (interactive) (drag-line-or-region-up 5)))

;; Moves line or region five lines down.
(global-set-key (kbd "<C-M-next>") '(lambda() (interactive) (drag-line-or-region-down 5)))