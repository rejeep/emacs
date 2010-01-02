;;; rejeep-javascript.el --- Javascript stuff

(add-hook 'javascript-mode-hook 'flyspell-prog-mode)
(add-hook 'javascript-mode
          '(lambda()
             (wrap-region-mode t)

             (setq javascript-indent-level 2)))

(provide 'rejeep-javascript)
