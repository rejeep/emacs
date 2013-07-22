(global-set-key (kbd "M-e") 'eshell)

(add-hook 'eshell-first-time-mode-hook
          (lambda ()
            (add-to-list 'eshell-visual-commands "htop")))

(setq eshell-history-size 5000)
(setq eshell-save-history-on-exit t)

(defun eshell/go-to-root ()
  (let ((root (find-project-root (eshell/pwd))))
    (if root (cd root))))

;; (defalias 'r 'eshell/go-to-root)

(provide 'rejeep-eshell)
