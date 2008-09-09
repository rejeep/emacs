;; Common programming stuff in this file.

(load "~/.emacs.d/rejeep/programming/ruby.el")
(load "~/.emacs.d/rejeep/programming/java.el")
(load "~/.emacs.d/rejeep/programming/rails.el")
(load "~/.emacs.d/rejeep/programming/javascript.el")
(load "~/.emacs.d/rejeep/programming/css.el")

(add-hook 'find-file-hooks
	  '(lambda ()
             ;; Make tab key to expand snippets.
	     (local-set-key (kbd "<tab>") 'yas/expand)

             ;; Activate highline.
             (highline-mode 1)
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

;; Style current line.
(require 'highline)

;; Sawfish
(autoload 'sawfish-mode "sawfish" "sawfish-mode" t)
(setq auto-mode-alist (cons '("\\.sawfishrc$"  . sawfish-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.jl$"         . sawfish-mode) auto-mode-alist))
