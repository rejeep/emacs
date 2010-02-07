;;; rejeep-elpa.el --- Elpa related

(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(defvar rejeep-packages (list 'haml-mode
                              'highlight-parentheses
                              'highlight-symbol
                              'htmlize
                              'javascript
                              'magit
                              'pastie
                              'sass-mode
                              'wrap-region
                              'yaml-mode)
  "Libraries that should be installed by default.")

(defun rejeep-elpa-install ()
  "Install all packages that aren't installed."
  (interactive)
  (dolist (package rejeep-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(provide 'rejeep-elpa)
