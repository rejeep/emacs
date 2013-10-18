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

;; Do not blink cursor
(blink-cursor-mode -1)

;; Do not show any tooltips
(tooltip-mode -1)

;; Remove selected region if typing
(pending-delete-mode 1)

;; Allow some commands
(dolist (command '(narrow-to-region downcase-region upcase-region))
  (put command 'disabled nil))

;; Prefer utf8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Set font size
(set-face-attribute 'default nil :height 140)

;; Do not ask for confirmation
(setq confirm-nonexistent-file-or-buffer nil)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Highlight symbol at point
(add-hook 'find-file-hook 'idle-highlight-mode)

(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Do not show annying menu-bar tips
(setq suggest-key-bindings nil)

;; I have tons of memory, stop the GC-madness.
(setq gc-cons-threshold 50000000)
(setq gc-cons-percentage 0.5)
