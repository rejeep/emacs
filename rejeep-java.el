;;; rejeep-java.el --- Java specific settings


(add-hook 'java-mode-hook
          (lambda()
            (c-set-style "bsd")
            (setq c-basic-offset 2)))


(provide 'rejeep-java)
