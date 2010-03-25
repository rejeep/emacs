;;; rejeep-comment.el --- Helpers for commenting/uncommenting stuff

(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments current current line or whole lines in
region (if any)."
  (interactive)
  (save-excursion
    (if mark-active
        (mark-whole-lines-region)
      (mark-current-line))
    (comment-or-uncomment-region (region-beginning) (region-end))))

(defun mark-current-line ()
  "Marks current line, except whitespace at the beginning."
  (interactive)
  (set-mark (line-end-position))
  (back-to-indentation))

(defun mark-whole-lines-region ()
  "Marks whole lines in the region."
  (if (> (point) (mark))
      (exchange-point-and-mark))
  (beginning-of-line)
  (exchange-point-and-mark)
  (end-of-line))

(provide 'rejeep-comment)
