;;; programming.el --- Programming stuff, not connected to any specific language.

(add-hook 'c-mode-common-hook
          (lambda () (c-subword-mode 1)))

(require 'haml-mode)
(require 'sass-mode)

(require 'markdown-mode)
(add-hook 'markdown-mode-hook
          '(lambda()
             (wrap-region-set-mode-punctuations '("[" "("))
             (wrap-region-mode t)
             ))

(add-hook 'magit-mode-hook 'rinari-launch)

(provide 'programming)