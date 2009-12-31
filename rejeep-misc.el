;;; rejeep-misc.el --- Stuff that don't fit anywhere else goes here

;; Custom file.
(setq custom-file "~/.emacs.d/rejeep-custom.el")
(load custom-file)

;; Hide menubar.
(menu-bar-mode nil)

;; Hide toolbar.
(tool-bar-mode nil)

;; Hide scrollbar.
(scroll-bar-mode nil)

;; Highlight the selected region.
(transient-mark-mode t)

;; Show information in minibuffer instead of as a tooltip.
(tooltip-mode nil)

;; Hitting delete will delete region and selecting a region and then
;; press a character will replace region with that character.
(pending-delete-mode 1)

;; Indent with spaces, instead of tabs.
(setq-default indent-tabs-mode nil)

;; No backup files
(setq make-backup-files nil)

;; kill-line will kill whole line (including empty line).
(setq kill-whole-line t)

;; Skip startup message.
(setq inhibit-startup-message t)

;; So that you only have to type y / n instead of yes / no.
(fset 'yes-or-no-p 'y-or-n-p)

;; Easy buffer switching by holding down shift and press any arrow key.
(windmove-default-keybindings 'shift)

;; Show unfinished keystrokes early.
(setq echo-keystrokes 0.1)

;; Maximum decoration
(setq font-lock-maximum-decoration t)

;; Use firefox as default browser.
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/bin/firefox")

;; Enable narrowing.
(put 'narrow-to-region 'disabled nil)

;; Highlight matching parenthesis.
(show-paren-mode 1)

;; Show empty lines at end of buffer.
(set-default 'indicate-empty-lines t)

;; For it's awesome rectangle mode.
(setq cua-enable-cua-keys nil)
(setq cua-toggle-set-mark nil)
(cua-mode)

;; Don't insert twice when no region is selected.
(setq wrap-region-insert-twice nil)

;; Run after wraping a region.
(add-hook 'wrap-region-after-hook
          '(lambda ()
             (indent-region wrap-region-beginning wrap-region-end)))

;; Paste buffer/region or get pastie.
(require 'pastie)

;; Prefer utf8.
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Indentation level by default is 2.
(setq-default c-basic-offset 2)

;; Use keats to handle keyboard shortcuts.
(setq keats-file "~/dev/emacs/keats")
(require 'keats)
(require 'keats-interactive)
(keats-mode t)

;; delete-trailing-whitespace is to long.
(defalias 'dtw 'delete-trailing-whitespace)

;; Add parts of each file's directory to the buffer name if not
;; unique.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Minimal finge.
(fringe-mode 'minimal)

;; Save place in files.
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; Allow up and down case of region.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Use the Twilight theme.
(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/packages/twilight/color-theme-twilight.el")
(color-theme-twilight)

;; Automatic and manual symbol highlighting.
(setq highlight-symbol-idle-delay 0.5)
(add-hook 'find-file-hook 'highlight-symbol-mode)


;; Highlight surrounding parentheses.
(add-hook 'find-file-hook 'highlight-parentheses-mode t)

;; Highlight current line in buffer.
(add-hook 'find-file-hook 'highline-mode-on)
(add-hook 'magit-mode-hook 'highline-mode-off)

;; Associate modes with file extensions.
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
(add-to-list 'auto-mode-alist '("\.markdown$" . markdown-mode))

(provide 'rejeep-misc)
