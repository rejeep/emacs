;; Common programming stuff in this file.

;; Wrap stuff.
(require 'wrap-region)

;; Some modes have on the fly syntax check enabled.
(require 'flymake)

;; Paste buffer/region or get pastie.
(require 'pastie)

;; Snippets.
(require 'yasnippet)
(yas/load-directory "~/.emacs.d/snippets")
(yas/initialize)

;; Load files for all modes.
(load "rejeep/programming/css.el")
(load "rejeep/programming/java/init.el")
(load "rejeep/programming/javascript.el")
(load "rejeep/programming/ruby.el")
(load "rejeep/programming/emacs-lisp.el")
(load "rejeep/programming/bash.el")

;; Emacs Code Browser.
(require 'ecb)

;; Sawfish
(autoload 'sawfish-mode "sawfish" "sawfish-mode" t)
(setq auto-mode-alist (cons '("\\.sawfishrc$"  . sawfish-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.jl$"         . sawfish-mode) auto-mode-alist))

;; Run after wraping a region.
(add-hook 'wrap-region-hook
	  '(lambda ()
             (indent-region wrap-region-beginning wrap-region-end)
             ))

;; Doc mode.
(require 'doc-mode)
(add-hook 'java-mode-hook 'doc-mode)