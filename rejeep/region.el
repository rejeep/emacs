(defun move-region-to-scratch-buffer()
  "Moves region to *scratch* buffer."
  (interactive)
  (append-to-buffer "*scratch*" (region-beginning) (region-end)))