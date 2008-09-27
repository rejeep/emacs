;; Contains functions that are used by other functions.

(defun region-selected()
  "Returns true if a region is selected. False otherwise."
  (and mark-active transient-mark-mode))

(defun move-region-to-buffer(buffer)
  (let ((prev-buffer (buffer-name)))    
    (kill-region (region-beginning) (region-end))
    (switch-to-buffer buffer)
    (end-of-buffer)
    (yank)
    (insert "\n")
    (switch-to-buffer prev-buffer)))