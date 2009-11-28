;;; javascript.el --- Javascript settings

(eval-after-load 'js-mode
  '(progn
     (wrap-region-mode t)

     (setq js-indent-level 2)
     ))

(provide 'javascript)