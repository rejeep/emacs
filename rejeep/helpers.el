;; Contains functions that are used by other functions.

(defun region-selected()
  "Returns true if a region is selected. False otherwise."
  (and mark-active transient-mark-mode))