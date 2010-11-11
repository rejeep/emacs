;;; rejeep-lisp.el --- All things Lisp


(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(require 'highlight-parentheses)
(add-hook 'find-file-hook 'highlight-parentheses-mode)

(define-key lisp-mode-shared-map (kbd "M-&") 'lisp-complete-symbol)

(add-to-list 'interpreter-mode-alist '("emacs" . emacs-lisp-mode))

(put 'ert-deftest 'lisp-indent-function 'defun)


(provide 'rejeep-lisp)
