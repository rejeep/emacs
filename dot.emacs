;; -*- mode: Emacs-Lisp -*-

(add-to-list 'load-path "~/.emacs.d/rejeep")

(require 'cl)

;; My custom stuff
(require 'loadpaths)
(require 'defuns)
(require 'bindings)
(require 'drag-stuff)
(require 'misc)

;; Programming
(require 'java)
(require 'ruby)
(require 'css)
(require 'javascript)
(require 'bash)
(require 'lisp)

;; (setq eshell-cmpl-cycle-completions nil
;;       eshell-save-history-on-exit t
;;       eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

;; (eval-after-load 'esh-opt
;;   '(progn
;;      (require 'em-prompt)
;;      (require 'em-term)
;;      (require 'em-cmpl)
;;      (setenv "PAGER" "cat")
;;      (set-face-attribute 'eshell-prompt nil :foreground "white")
;;      (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
;;                '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))
;;      (add-to-list 'eshell-visual-commands "ssh")
;;      (add-to-list 'eshell-visual-commands "tail")
;;      (add-to-list 'eshell-command-completions-alist
;;                   '("gunzip" "gz\\'"))
;;      (add-to-list 'eshell-command-completions-alist
;;                   '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))
;;      (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)))
