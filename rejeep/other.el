;; Put stuff here that don't fit anywhere else.

(defun google-region()
  "Google the selected region."
  (interactive)
  (browse-url (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
                      (buffer-substring (region-beginning) (region-end)))))