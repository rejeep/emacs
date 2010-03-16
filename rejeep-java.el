;;; rejeep-java.el --- Java specific settings

(add-hook 'java-mode-hook 'flyspell-prog-mode)
(add-hook 'java-mode-hook
          '(lambda()
             ;; Set code style
             (c-set-style "bsd")

             ;; Indent width is two spaces.
             (setq c-basic-offset 2)))

(provide 'rejeep-java)
