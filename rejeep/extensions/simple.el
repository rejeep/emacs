(defun open-line-below()
  "Open a line below the line the point is at.
Then move to that line and indent accordning to mode"
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

(defun open-line-above()
  "Open a line above the line the point is at.
Then move to that line and indent accordning to mode"
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (previous-line)
  (indent-according-to-mode))

(defun mark-current-word()
  "Mark the word cursor is on."
  (interactive)
  (backward-word)
  (set-mark (point))
  (forward-word))

(defun back-to-indentation-or-beginning-of-line()
  "Moves point back to indentation if there is any
non blank characters to the left of the cursor.
Otherwise point moves to beginning of line."
  (interactive)
  (if (or (bolp) (string-match "^[ \t]*$" (buffer-substring (line-beginning-position) (point))))
      (beginning-of-line)
    (back-to-indentation)))