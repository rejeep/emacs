;;; init.el - Where it all begins

;; Turn off early to avoid momentary display.
(mapc
 (lambda (mode)
   (if (fboundp mode)
       (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))

(add-to-list 'load-path user-emacs-directory)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)

(require 's)
(require 'dash)
(require 'f)
(require 'misc)
(require 'rejeep-ido)
(require 'rejeep-defuns)
(require 'rejeep-misc)
(require 'rejeep-bindings)
(require 'rejeep-programming)

(if (eq system-type 'darwin)
    (require 'rejeep-osx))



