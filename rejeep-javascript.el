;;; rejeep-javascript.el --- Javascript stuff

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(add-hook 'js2-mode-hook
          (lambda()
            (define-key js2-mode-map (kbd "C-a") nil)
            (define-key js2-mode-map (kbd "C-M-h") nil)
            (define-key js2-mode-map (kbd "M-j") nil)
            (setq js2-basic-offset 2)))

(add-hook 'js-mode-hook
          (lambda()
            (setq js-indent-level 2)))

(add-hook 'coffee-mode-hook
          (lambda ()
            (setq coffee-tab-width 2)
            (setq coffee-cleanup-whitespace nil)
            
            (define-key coffee-mode-map (kbd "C-j") 'coffee-newline-and-indent)))

(dolist (regex '("Jakefile$"))
  (add-to-list 'auto-mode-alist `(,regex . js2-mode)))


(provide 'rejeep-javascript)
