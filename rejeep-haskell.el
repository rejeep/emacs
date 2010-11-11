;;; rejeep-haskell.el --- Haskell stuff


(load (expand-file-name "haskell-site-file" (expand-file-name "haskell-mode" vendor-dir)))

(defun haskell-insert-module-definition ()
  "Insert module definition if file is empty."
  (let ((file (buffer-file-name)))
    (when (and file (= (buffer-size) 0))
      (insert (format "module %s where\n\n" (file-name-sans-extension (file-name-nondirectory file)))))))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'haskell-insert-module-definition)
(add-hook 'haskell-mode-hook
          (lambda ()
            (define-key haskell-mode-map (kbd "M-n")
              (lambda ()
                (interactive)
                (move-end-of-line 1)
                (newline)))

            (define-key haskell-mode-map (kbd "M-p")
              (lambda ()
                (interactive)
                (move-beginning-of-line 1)
                (newline)
                (forward-line -1)))
            ))


(provide 'rejeep-haskell)
