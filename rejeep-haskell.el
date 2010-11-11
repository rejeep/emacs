;;; rejeep-haskell.el --- Haskell stuff


(load (expand-file-name "haskell-site-file" (expand-file-name "haskell-mode" vendor-dir)))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(add-hook 'haskell-mode-hook
          (lambda ()
            (define-key haskell-mode-map (kbd "M-n")
              (lambda ()
                (interactive)
                (move-end-of-line 1)
                (newline)))))

(add-hook 'haskell-mode-hook
          (lambda ()
            (define-key haskell-mode-map (kbd "M-p")
              (lambda ()
                (interactive)
                (move-beginning-of-line 1)
                (newline)
                (forward-line -1)))))

(add-hook 'find-file-hook
          (lambda ()
            (interactive)
            (let ((file (buffer-file-name)))
              (if (and file (equal major-mode 'haskell-mode) (= (- (point-max) (point-min)) 0))
                  (insert (format "module %s where\n\n" (file-name-sans-extension (file-name-nondirectory file))))))))


(provide 'rejeep-haskell)
