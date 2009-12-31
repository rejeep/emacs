;;; rejeep-elpa.el --- Elpa related

(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(defvar starter-kit-packages (list 'haml-mode
                                   'light-symbol
                                   'sass-mode
                                   'yaml-mode
                                   'css-mode)
  "Libraries that should be installed by default.")
 
(defun starter-kit-elpa-install ()
  "Install all starter-kit packages that aren't installed."
  (interactive)
  (dolist (package starter-kit-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))
 
(defun esk-online? ()
  "See if we're online.
 
Windows does not have the network-interface-list function, so we
just have to assume it's online."
  ;; TODO how could this work on Windows?
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

(provide 'rejeep-elpa)
 