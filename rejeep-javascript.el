;;; rejeep-javascript.el --- Javascript stuff


(add-hook 'js-mode-hook 'flyspell-prog-mode)
(add-hook 'js-mode-hook
          '(lambda()
             (setq js-indent-level 2)))

(provide 'rejeep-javascript)
