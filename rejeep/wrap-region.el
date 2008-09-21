(defvar wrap-characters '(("\"" . "\"")
                          ("'" . "'")
                          ("(" . ")")
                          ("{" . "}")
                          ("[" . "]")
                          ("<" . ">")
                          ("|" . "|"))
  "Characters and their right 'friend'.")

(defun wrap-region(left right beg end)
  "Wraps a region with left and right."
  (save-excursion
    (goto-char beg)
    (insert left)
    (goto-char (+ end (length left)))
    (insert right)))

(defun wrap-region-or-insert(left)
  "Wraps a region with a region with punctuation characters
or inserts the characters and places the cursor in between them."
  (interactive)
  (let ((right (cdr (assoc left wrap-characters))))
    (if (region-selected)
        (wrap-region left right (region-beginning) (region-end))
      (insert (concat left right)) (backward-char))))

(defun wrap-region-with-tag-or-insert()
  "Wraps a region with a tag or insert a < if no region is selected."
  (interactive)
  (if (region-selected)
      (call-interactively 'wrap-region-with-tag)
    (insert "<")))

(defun wrap-region-with-tag(tag beg end)
  "Wraps a region with a tag."
  (interactive "*sTag: \nr")
  (wrap-region (concat "<" tag ">") (concat "</" tag ">") beg end))

(provide 'wrap-region)