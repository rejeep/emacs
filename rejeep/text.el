(defun backward-delete-word()
  (interactive)
  (delete-region (point) (progn (backward-word) (point))))

(defun mark-current-word()
  "Mark the word cursor is on."
  (interactive)
  (backward-word)
  (set-mark (point))
  (forward-word))

(defun move-region-to-scratch-buffer()
  "Moves region to *scratch* buffer."
  (interactive)
  (let ((prev-buffer (buffer-name)))    
    (kill-region (point) (mark))
    (switch-to-buffer "*scratch*")
    (end-of-buffer)
    (yank)
    (insert "\n")
    (switch-to-buffer prev-buffer)))