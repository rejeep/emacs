;;; rejeep-osx.el --- OSX specific settings

(when (eq system-type 'darwin)

  ;; Solve Meta-key problem
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)

  ;; Menu bar is not cluttering the view in OSX.
  (menu-bar-mode 1)

  ;; Make Ido ignore freaking .DS_Store files.
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  
  ;; Make the browser the OS X default.
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  
  ;; In dired, move deletions to trash.
  (setq delete-by-moving-to-trash t)
  )

(provide 'rejeep-osx)
