;;; rejeep-bash.el --- (ba)sh specific settings

(add-hook 'sh-mode-hook 'wrap-region-mode)

(eval-after-load 'sh-script
  '(progn
     (setq sh-indentation 2)))

(provide 'rejeep-bash)
