;;; rejeep-lisp.el --- All that has to do with (any dialect of) Lisp

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'wrap-region-mode)
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)))

(provide 'rejeep-lisp)
