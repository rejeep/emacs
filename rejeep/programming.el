;;; programming.el --- Programming stuff, not connected to any specific language.

(add-hook 'c-mode-common-hook
          (lambda () (c-subword-mode 1)))

(require 'haml-mode)
(require 'sass-mode)
(require 'markdown-mode)

(provide 'programming)