(defvar wrap-characters '(("\"" . "\"")
                          ("'" . "'")
                          ("(" . ")")
                          ("{" . "}")
                          ("[" . "]")
                          ("<" . ">")
                          ("|" . "|"))
  "Characters and their right 'friend'.")

(defvar wrap-region-hook
  "Runned after a region is wrapped. These variables are set:
wrap-region-beginning: The begining of the region before wrap.
wrap-region-end:The end of the region before wrap."
  '()
  )

(defun wrap-region(left right beg end)
  "Wraps a region with left and right."
  (save-excursion
    (goto-char beg)
    (insert left)
    (goto-char (+ end (length left)))
    (insert right)
    (let ((wrap-region-beginning beg) (wrap-region-end end))
      (run-hooks 'wrap-region-hook)
      )))

(defun wrap-region-with-function(left)
  "Returns a function which, when called, will interactively `wrap-region-or-insert'."
  (interactive)
  `(lambda() (interactive)
     (wrap-region-or-insert ,left)))

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

(defun wrap-region-with-tag(tag)
  "Wraps a region with a tag."
  (interactive "*sTag: ")
  (wrap-region (concat "<" tag ">") (concat "</" tag ">") (region-beginning) (region-end)))

(defun wrap-region-bind-keys(mode-map &rest punctuations)
  "Set wrapper key bindings easy."
  (dolist (punctuation punctuations)
    (define-key mode-map punctuation (wrap-region-with-function punctuation))))

(provide 'wrap-region)