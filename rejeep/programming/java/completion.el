(defun java-complete()
  "Choose method completion using ido-mode and Yasnippet (if method has one or more arguments)."
  (interactive)
  (let ((completion-list (completion-list)))
    (if (not (empty completion-list))
        (let ((choise))
          (setq choise (ido-completing-read "> " completion-list))
          (delete-back-to-period)
          (let ((method))
            (unless (string-match "^.*()$" choise)
              (progn
                (setq method (replace-regexp-in-string "\(" "(${" choise))
                (setq method (replace-regexp-in-string ", " "}, ${" method))
                (setq method (replace-regexp-in-string "\)" "})" method))))
            (if method
                (yas/expand-snippet (point) (point) method)
              (insert choise))))
      (message "No completions at point..."))))

(defun completion-list()
  "Returns a list of all posible completions at point."
  (let* ((pair (jde-parse-java-variable-at-point)) (access (jde-complete-get-access pair)) (completion-list))
    (if access
        (setq completion-list (jde-complete-find-completion-for-pair pair nil access))
      (setq completion-list (jde-complete-find-completion-for-pair pair)))
    (if (null completion-list)
        (setq completion-list (jde-complete-find-completion-for-pair
                               (list (concat "this." (car pair)) "")
                               nil jde-complete-private)))
    (if (null completion-list)
        (setq completion-list (jde-complete-find-completion-for-pair
                               (list (concat "super." (car pair)) "")
                               nil jde-complete-protected)))
    (values completion-list)))