(defun indent-region-or-buffer()
  "Indents region if any. Otherwise whole buffer."
  (interactive)
  (if (region-selected)
      (call-interactively 'indent-region)
    (indent-buffer)))

(defun indent-buffer()
  "Indents whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))