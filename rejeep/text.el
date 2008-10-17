(defun backward-delete-word()
  (interactive)
  (delete-region (point) (progn (backward-word) (point))))

(defun mark-current-word()
  "Mark the word cursor is on."
  (interactive)
  (backward-word)
  (set-mark (point))
  (forward-word))

(defun comment-or-uncomment-whole-lines-region()
  "Comments or uncomments whole lines in the selected region
or on current line if no region is selected."
  (interactive)
  (save-excursion
    (if (region-selected)
        (mark-whole-lines-region (region-beginning) (region-end))
      (mark-current-line))
    (comment-or-uncomment-region (region-beginning) (region-end))))