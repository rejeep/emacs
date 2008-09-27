(defun rails-find-model()
  (interactive)
  (rails-find "app/models"))

(defun rails-find-view()
  (interactive)
  (rails-find "app/views"))

(defun rails-find-controller()
  (interactive)
  (rails-find "app/controllers"))

(defun rails-find-helper()
  (interactive)
  (rails-find "app/helper"))

(defun rails-find-migration()
  (interactive)
  (rails-find "db/migrate"))

(defun rails-find(location)
  (ido-find-file-in-dir (concat (rinari-root) "/" location)))