;; Contains functions that are used by other functions.

(defun region-selected()
  "Returns true if a region is selected. False otherwise."
  (and mark-active transient-mark-mode))

(defun uniq(list)
  "Given a list it returns that list but with unique elements."
  (let ((temp))
    (dolist (element list)
      (unless (find element temp)
        (add-to-list 'temp element)))
    temp))

(defun keys(alist)
  "Returns a list of all keys in the alist."
  (let ((temp))
    (dolist (element alist)
      (add-to-list 'temp (car element)))
    temp))

(defun values(alist)
  "Returns a list of all values in the alist."
  (let ((temp))
    (dolist (element alist)
      (add-to-list 'temp (cdr element)))
    temp))

(defun empty(list)
  "Returns true if list contains no elements. nil otherwise."
  (= (length list) 0))