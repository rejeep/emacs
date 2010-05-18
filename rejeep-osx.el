;;; rejeep-osx.el --- OSX specific settings

(when (eq system-type 'darwin)

  ;; Solve Meta-key problem
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)

  ;; Menu bar is not cluttering the view in OSX.
  (menu-bar-mode 1)

  )

(provide 'rejeep-osx)
