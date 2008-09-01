(load "~/.emacs.d/rejeep/programming/ruby.el")
(load "~/.emacs.d/rejeep/programming/java.el")

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