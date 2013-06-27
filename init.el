;;; init.el - Where it all begins

(defconst vendor-dir
  (expand-file-name "vendor" user-emacs-directory)
  "Path to vendor directory.")

;; Turn off early to avoid momentary display.
(mapc
 (lambda (mode)
   (if (fboundp mode)
       (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))

(load (expand-file-name "rejeep-paths.el" user-emacs-directory))

(package-initialize)

(require 's)
(require 'dash)
(require 'misc)
(require 'rejeep-ido)
(require 'rejeep-defuns)
(require 'rejeep-misc)
(require 'rejeep-bindings)
(require 'rejeep-programming)

(if (eq system-type 'darwin)
    (require 'rejeep-osx))

(require 'carton)
(carton-setup user-emacs-directory)


