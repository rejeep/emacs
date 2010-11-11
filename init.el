;;; init.el - Where it all begins


(setq emacs-dir (file-name-directory
                 (or (buffer-file-name) load-file-name)))
(setq vendor-dir (expand-file-name "vendor" emacs-dir))

;; Turn off early to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (if (fboundp mode) (funcall mode -1)))

(load (expand-file-name "rejeep-paths.el" emacs-dir))

(require 'cl)
(require 'misc)
(require 'rejeep-ido)
(require 'rejeep-defuns)
(require 'rejeep-misc)
(require 'rejeep-bindings)
(require 'rejeep-programming)

(cond ((eq system-type 'darwin)
       (require 'rejeep-osx)))