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
(load "rejeep/programming/css")
(load "rejeep/programming/java/init")
(load "rejeep/programming/javascript")
(load "rejeep/programming/ruby")
(load "rejeep/programming/emacs-lisp")
(load "rejeep/programming/bash")

;; Emacs Code Browser.
(require 'ecb)

;; Run after wraping a region.
(add-hook 'wrap-region-hook
	  '(lambda ()
             (indent-region wrap-region-beginning wrap-region-end)
             ))

;; Doc mode.
(require 'doc-mode)
(add-hook 'java-mode-hook 'doc-mode)