;;; bindings.el --- Set up of key bindings

;; Go to line.
(global-set-key (kbd "M-g") 'goto-line)

;; Expandations.
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-_") 'dabbrev-expand)

;; Copy region to clippboard.
(global-set-key (kbd "C-x M-w") 'clipboard-kill-ring-save)

;; Yank from clippboard.
(global-set-key (kbd "C-x M-v") 'clipboard-yank)

;; Newline and then indent.
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)

;; Open line below and go to that line.
(global-set-key (kbd "C-n") 'open-line-below)

;; Open line above and go to that line.
(global-set-key (kbd "C-p") 'open-line-above)

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

;; Back to indentation or beginning of line.
(global-set-key (kbd "<home>") 'back-to-indentation-or-beginning-of-line)

;; Delete work backwards without saving it to the kill ring.
(global-set-key (kbd "<M-backspace>") 'backward-delete-word)

;; Google region.
(global-set-key (kbd "C-c C-c g") 'google-region)

;; Comments or uncomments a line or region
(global-set-key (kbd "C-7") 'comment-or-uncomment-whole-lines-region)

;; Moves region to *scratch* buffer.
(global-set-key (kbd "C-c s") 'copy-region-to-scratch-buffer)

;; Moves line or region one line down.
(global-set-key (kbd "<C-M-down>") 'drag-line-or-region-down)

;; Moves line or region one line up.
(global-set-key (kbd "<C-M-up>") 'drag-line-or-region-up)

;; Show occurances of regexp.
(global-set-key (kbd "C-o") 'occur)

;; Magit status.
(global-set-key (kbd "C-x g") 'magit-status)

;; Move cursor 30 lines down or to end of buffer.
(global-set-key (kbd "<next>")
                (lambda ()
                  (interactive)
                  (forward-line 30)))

;; Move cursor 30 lines up or to beginning of buffer.
(global-set-key (kbd "<prior>")
                (lambda ()
                  (interactive)
                  (forward-line -30)))

;; Move five rows up.
(global-set-key (kbd "<C-up>")
                (lambda ()
                  (interactive)
                  (next-line -5)))

;; Move five rows down.
(global-set-key (kbd "<C-down>")
                (lambda ()
                  (interactive)
                  (next-line 5)))

;; So that Emacs never is quit by mistake.
(global-set-key (kbd "C-x C-c")
                (lambda ()
                  (interactive)
                  (if (y-or-n-p "Quit Emacs? ")
                      (save-buffers-kill-emacs))))

;; Moves line or region five lines up.
(global-set-key (kbd "<C-M-prior>")
                (lambda ()
                  (interactive)
                  (drag-line-or-region-up 5)))

;; Moves line or region five lines down.
(global-set-key (kbd "<C-M-next>")
                (lambda ()
                  (interactive)
                  (drag-line-or-region-down 5)))

;; Perform general cleanup of buffer.
(global-set-key (kbd "C-c n")
                (lambda ()
                  (interactive)
                  (indent-buffer)
                  (delete-trailing-whitespace)
                  (untabify-buffer)))

(provide 'bindings)