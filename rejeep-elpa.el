;;; rejeep-elpa.el --- Elpa related

(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(defvar rejeep-packages '(drag-stuff
                          haml-mode
                          highlight-parentheses
                          highlight-symbol
                          html-script-src
                          htmlize
                          magit
                          pastie
                          sass-mode
                          wrap-region
                          yaml-mode)
  "Libraries that should be installed from ELPA.")

(defun rejeep-elpa-install ()
  "Install all packages that aren't installed."
  (interactive)
  (dolist (package rejeep-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(provide 'rejeep-elpa)
