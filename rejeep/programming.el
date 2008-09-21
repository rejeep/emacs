;; Common programming stuff in this file.

;; Some modes have on the fly syntax check enabled.
(require 'flymake)

;; Load files for all modes.
(load "rejeep/programming/css.el")
(load "rejeep/programming/java.el")
(load "rejeep/programming/javascript.el")
(load "rejeep/programming/ruby.el")

;; Hook for opening files.
(add-hook 'find-file-hooks
	  '(lambda ()
             ;; Make tab key to expand snippets.
	     (local-set-key (kbd "<tab>") 'yas/expand)
             ))

;; Snippets.
(require 'yasnippet)
(yas/load-directory "~/.emacs.d/snippets")
(yas/initialize)

(add-hook 'yas/after-exit-snippet-hook
	  '(lambda ()
             ;; Indent whole snippet region.
             (indent-region yas/snippet-beg yas/snippet-end)
             ))

;; Emacs Code Browser.
(require 'ecb)

;; Sawfish
(autoload 'sawfish-mode "sawfish" "sawfish-mode" t)
(setq auto-mode-alist (cons '("\\.sawfishrc$"  . sawfish-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.jl$"         . sawfish-mode) auto-mode-alist))
