;;; programming.el --- Programming stuff, not connected to any specific language.

(add-hook 'c-mode-common-hook
          (lambda () (c-subword-mode 1)))

(provide 'programming)