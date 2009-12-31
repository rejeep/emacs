;;; rejeep-java.el --- Java specific settings

(add-hook 'java-mode-hook
          '(lambda()
             (wrap-region-mode t)

             ;; Set code style
             (c-set-style "bsd")

             ;; Indent width is two spaces.
             (setq c-basic-offset 2)))

(provide 'rejeep-java)