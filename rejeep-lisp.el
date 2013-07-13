;;; rejeep-lisp.el --- All things Lisp

(require 'highlight-parentheses)
(add-hook 'find-file-hook
          (lambda ()
            (interactive)
            (highlight-parentheses-mode +1)))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("(\\(\\<ert-deftest\\)\\>\\s *\\(\\sw+\\)?"
                (1 font-lock-keyword-face nil t)
                (2 font-lock-function-name-face nil t))))))

(define-key lisp-mode-shared-map (kbd "M-&") 'lisp-complete-symbol)
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

(add-to-list 'interpreter-mode-alist '("emacs" . emacs-lisp-mode))

(add-to-list 'auto-mode-alist '("Carton" . emacs-lisp-mode))

(put 'ert-deftest 'lisp-indent-function 'defun)

(provide 'rejeep-lisp)
