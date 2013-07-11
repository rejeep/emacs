;;; rejeep-osx.el --- OSX specific settings


;; Switch the Cmd and Meta keys
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; Menu bar is not annoying in OSX
(menu-bar-mode 1)

;; Make Ido ignore freaking .DS_Store files
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Make the browser the OS X default
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; In dired, move deletions to trash
(setq delete-by-moving-to-trash t)

;; Set font
(set-default-font "-apple-Source_Code_Pro-medium-normal-normal-*-*-*-*-*-m-0-iso10646-1")

(defun finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file."))))

;; Use GNU ls - install with:
;;    brew install xz
;;    brew install coreutils
(setq insert-directory-program "gls")

(exec-path-from-shell-initialize)

(provide 'rejeep-osx)
