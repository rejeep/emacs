;;; rejeep-css.el --- CSS specific settings.

(eval-after-load 'css-mode
  (progn
    (setq css-indent-offset 2)
    (font-lock-add-keywords 'css-mode
                            '(("#[a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\([a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\)?" . font-lock-reference-face)
                              ("[0-9]+\\(px\\|em\\|%\\)" 1 font-lock-keyword-face)
                              ("\\(url\\)(" 1 font-lock-function-name-face)))))

(provide 'rejeep-css)
