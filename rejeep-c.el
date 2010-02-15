;;; rejeep-c.el --- c programming stuff

(add-hook 'c-mode-hook
          (lambda()
            (c-set-style "K&R")
            (setq tab-width 2)
            (setq c-basic-offset 2)))

(provide 'rejeep-c)
