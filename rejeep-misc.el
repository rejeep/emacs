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
(cua-mode 1)

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

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Save cursor position
(require 'saveplace)
(setq-default save-place t)

;; Color theme
(load-theme 'zenburn t)

;; Highlight symbol at point
(add-hook 'find-file-hook 'idle-highlight-mode)

;; Wrap Region
(require 'wrap-region)
(wrap-region-global-mode 1)
(wrap-region-add-wrappers
 '(("`" "'" nil 'emacs-lisp-mode)
   ("$" "$" nil 'latex-mode)
   ("{-" "-}" "#" 'haskell-mode)
   ("/" "/" nil 'ruby-mode)
   ("/* " " */" "#" '(java-mode javascript-mode css-mode))
   ("`" "`" nil '(markdown-mode ruby-mode))
   ("``" "''" "\"" 'latex-mode)))

;; Drag Stuff
(require 'drag-stuff)
(add-to-list 'drag-stuff-except-modes 'org-mode)
(drag-stuff-global-mode 1)

(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Projectile
(projectile-global-mode)
(setq projectile-enable-caching t)
(add-to-list 'projectile-globally-ignored-files ".DS_Store")
(setq projectile-require-project-root nil)

;; Popwin
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;; Ack
(autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; Smex
(require 'smex)
(smex-initialize)

;; Multiple Cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Diff HL
(add-hook 'vc-checkin-hook 'diff-hl-update)
(global-diff-hl-mode)

;; Make projectile search usable
(setq projectile-completion-system 'projectile-completion-fn)

;; Nyan
(nyan-mode 1)

(provide 'rejeep-misc)
