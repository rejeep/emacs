;;; rejeep-css.el --- CSS specific settings


(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)))

(font-lock-add-keywords
 'css-mode
 '(("#[a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\([a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\)?" . font-lock-reference-face)
   ("\\([0-9]*\\.?[0-9]+\\)\\(?:px\\|em\\|%\\)" 1 font-lock-keyword-face)))


(provide 'rejeep-css)
