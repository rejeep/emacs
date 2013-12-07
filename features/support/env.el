(require 'f)
(require 'ert)
(require 'espuds)

(load (f-expand "defuns" user-emacs-directory) nil t)

(Before
 (switch-to-buffer
  (get-buffer-create "*emacs*"))
 (erase-buffer))
