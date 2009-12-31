;;; rejeep-javascript.el --- Javascript stuff

(eval-after-load 'javascript-mode
  '(progn
     (wrap-region-mode t)

     (setq javascript-indent-level 2)))

(provide 'rejeep-javascript)