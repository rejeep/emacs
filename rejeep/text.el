(defun backward-delete-word()
  (interactive)
  (delete-region (point) (progn (backward-word) (point))))

(defun mark-current-word()
  "Mark the word cursor is on."
  (interactive)
  (backward-word)
  (set-mark (point))
  (forward-word))