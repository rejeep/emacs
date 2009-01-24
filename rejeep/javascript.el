;;; javascript.el --- Javascript settings

(autoload #'espresso-mode "espresso" "Start espresso-mode" t)

(eval-after-load 'espresso-mode
  '(progn
     (defconst flymake-allowed-js-file-name-masks '(("\\.json$" flymake-js-init)
                                                    ("\\.js$" flymake-js-init)))
     (defcustom flymake-js-detect-trailing-comma t nil :type 'boolean)
     (defvar flymake-js-err-line-patterns '(("^\\(.+\\)\:\\([0-9]+\\)\: \\(SyntaxError\:.+\\)\:$" 1 2 nil 3)))
     (when flymake-js-detect-trailing-comma
       (setq flymake-js-err-line-patterns (append flymake-js-err-line-patterns
                                                  '(("^\\(.+\\)\:\\([0-9]+\\)\: \\(strict warning: trailing comma.+\\)\:$" 1 2 nil 3)))))


     (defun flymake-js-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "js" (list "-s" local-file))))

     (defun flymake-js-load ()
       (interactive)
       (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
         (setq flymake-check-was-interrupted t))
       (ad-activate 'flymake-post-syntax-check)
       (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-js-file-name-masks))
       (setq flymake-err-line-patterns flymake-js-err-line-patterns)
       (flymake-mode t))

     (add-hook 'espresso-mode-hook '(lambda () (flymake-js-load)))
     ))

(add-hook 'espresso-mode-hook
          '(lambda()
             (wrap-region-mode t)
             ))

(provide 'javascript)