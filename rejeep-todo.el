(require 'f)

(defvar rejeep-todo-path (f-join "~" "Dropbox" "todo"))

(defun rejeep-todo (arg)
  (interactive "P")
  (let* ((project-root
          (when (and (buffer-file-name) (not arg))
            (find-project-root (buffer-file-name))))
         (project-name
          (when project-root (f-no-ext (f-filename (f-canonical project-root)))))
         (todo-file
          (f-expand (concat (or project-name "global") ".org") rejeep-todo-path)))
    (unless (f-file? todo-file)
      (f-touch todo-file))
    (find-file todo-file)))

(global-set-key (kbd "C-c C-n") 'rejeep-todo)

(provide 'rejeep-todo)
