;;; drag-stuff.el --- Helpers that moves a region or a line up and down in a buffer

(defun drag-region(arg)
  "Drags a region up or down."
  (if (> (point) (mark))
      (exchange-point-and-mark))
  (let ((column (current-column))
        (text (delete-and-extract-region (region-beginning) (region-end))))
    (forward-line arg)
    (move-to-column column t)
    (set-mark (point))
    (insert text)
    (exchange-point-and-mark)
    (setq deactivate-mark nil)))

(defun drag-line(arg)
  "Drag a line up or down."
  (let ((column (current-column)))
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1))
    (move-to-column column t)))

(defun drag-line-or-region(arg)
  "Drags a region if any region selected.
Otherwise line at point will be dragged."
  (if mark-active
      (drag-region arg)
    (drag-line arg)))

(defun drag-line-or-region-down(arg)
  "Drag line or region down ARG rows."
  (interactive "*p")
  (drag-line-or-region arg))

(defun drag-line-or-region-up(arg)
  "Drag line or region up ARG rows."
  (interactive "*p")
  (drag-line-or-region (- arg)))

(provide 'drag-stuff)