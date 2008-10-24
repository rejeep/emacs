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
    (mark-current-line-or-whole-lines-region)
    (comment-or-uncomment-region (region-beginning) (region-end))))

(defun copy-and-comment-line-or-region()
  "Takes whole lines in region if any region is selected,
otherwise current line, comments it out and pastes an uncomment copy below."
  (interactive)
  (let ((column (current-column)) (text))
    (save-excursion
      (mark-current-line-or-whole-lines-region)
      (setq text (buffer-substring (region-beginning) (region-end)))
      (comment-or-uncomment-whole-lines-region)
      (goto-char (region-end))
      (insert (concat "\n" text)))
    (move-to-column column t)))

(defun kill-region-or-current-word()
  "Kills a region if selected. Otherwise current word is deleted."
  (interactive)
  (unless (region-selected)
    (mark-current-word))
  (kill-region (region-beginning) (region-end)))