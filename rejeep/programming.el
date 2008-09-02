(load "~/.emacs.d/rejeep/programming/ruby.el")
(load "~/.emacs.d/rejeep/programming/java.el")
(load "~/.emacs.d/rejeep/programming/rails.el")

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

;; Emacs Code Browser.
(require 'ecb)

;; Style current line.
(require 'highline)