;;; rejeep-javascript.el --- Javascript stuff

(add-hook 'javascript-mode-hook 'flyspell-prog-mode)
(add-hook 'javascript-mode 'wrap-region-mode)
(add-hook 'javascript-mode
          '(lambda()
             (setq javascript-indent-level 2)))

(provide 'rejeep-javascript)
