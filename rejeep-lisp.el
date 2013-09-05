;;; rejeep-lisp.el --- All things Lisp

(require 'eldoc)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map (kbd "M-q")
              (lambda ()
                (interactive)
                (save-excursion
                  (end-of-defun)
                  (beginning-of-defun)
                  (indent-sexp))))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("(\\(\\<ert-deftest\\)\\>\\s *\\(\\sw+\\)?"
                (1 font-lock-keyword-face nil t)
                (2 font-lock-function-name-face nil t))))))

(eldoc-add-command 'paredit-backward-delete 'paredit-close-round)

(define-key lisp-mode-shared-map (kbd "M-&") 'lisp-complete-symbol)
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

(add-to-list 'interpreter-mode-alist '("emacs" . emacs-lisp-mode))

(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))

(put 'ert-deftest 'lisp-indent-function 'defun)

(define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)

(provide 'rejeep-lisp)
