;;; rejeep-flymake.el --- Flymake stuff

(require 'flymake)

;; Default spelling dictionary.
(setq ispell-dictionary "english")

;; Add flyspell to modes.
(add-hook 'ruby-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'java-mode-hook 'flyspell-prog-mode)
(add-hook 'js2-mode-hook 'flyspell-prog-mode)
(add-hook 'magit-log-edit-mode-hook 'flyspell-mode)
(add-hook 'yaml-mode-hook 'flyspell-mode)

(provide 'rejeep-flymake)