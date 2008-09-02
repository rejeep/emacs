(defun backward-delete-word()
  (interactive)
  (delete-region (point) (progn (backward-word) (point))))