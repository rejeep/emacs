;;; rejeep-css.el --- CSS specific settings.

(autoload 'css-mode "css-mode")
(setq auto-mode-alist (cons '("\\.css\\'" . css-mode) auto-mode-alist))

(eval-after-load 'css-mode
  '(progn
     (setq css-indent-offset 2)))

(provide 'rejeep-css)
