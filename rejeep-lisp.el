;;; rejeep-lisp.el --- All that has to do with (any dialect of) Lisp

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)

(define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)

(add-to-list 'interpreter-mode-alist '("emacs" . emacs-lisp-mode))

(provide 'rejeep-lisp)
