;;; rejeep-misc.el --- Miscellaneous settings


;; Do not pause on redisplay
(setq redisplay-dont-pause t)

;; Do not make any backup files
(setq make-backup-files nil)

;; Kill whole line
(setq kill-whole-line t)

;; Do not show startup message
(setq inhibit-startup-message t)

;; Show keystrokes in minibuffer early
(setq echo-keystrokes 0.1)

;; Set default browser
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "google-chrome")

;; Initial major mode is Emacs Lisp mode
(setq initial-major-mode 'emacs-lisp-mode)

;; Indent with spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Truncate lines
(set-default 'truncate-lines t)

;; Show matching parenthesis
(show-paren-mode 1)

;; Do not blink cursor
(blink-cursor-mode -1)

;; Do not show any tooltips
(tooltip-mode -1)

;; Remove selected region if typing
(pending-delete-mode 1)

;; Shift + arrow key move cursor to buffer in arrow direction
(windmove-default-keybindings 'shift)

;; Allow some commands
(dolist (command '(narrow-to-region downcase-region upcase-region))
  (put command 'disabled nil))

;; CUA rectangle mode
(setq cua-enable-cua-keys nil)
(setq cua-toggle-set-mark nil)
(cua-mode)

;; Prefer utf8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Set font size
(set-face-attribute 'default nil :height 110)

;; Smooth scrolling
(require 'smooth-scrolling)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)

;; Do not ask for confirmation
(setq confirm-nonexistent-file-or-buffer nil)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Save cursor position
(require 'saveplace)
(setq-default save-place t)

;; Color theme
(require 'color-theme)
(require 'color-theme-rejeep)
(color-theme-initialize)
(color-theme-rejeep)

;; Highlight symbol at point
(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.5)
(add-hook 'find-file-hook 'highlight-symbol-mode)

;; Wrap Region
(require 'wrap-region)
(wrap-region-global-mode 1)

;; Drag Stuff
(require 'drag-stuff)
(add-to-list 'drag-stuff-except-modes 'org-mode)
(drag-stuff-global-mode 1)

(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'yes-or-no-p 'y-or-n-p)


(provide 'rejeep-misc)