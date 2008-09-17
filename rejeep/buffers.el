;; Contains functions related to buffers.

(defun nuke-all-buffers()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapcar (lambda(x) (kill-buffer x)) (buffer-list))
  (delete-other-windows))