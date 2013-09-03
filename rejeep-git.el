;;; rejeep-git.el --- Git from Emacs

(require 'magit)
(require 'magit-blame)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
(setq magit-set-upstream-on-push t)

(setq magit-completing-read-function 'magit-ido-completing-read)

(defun magit-toggle-process-window ()
  "Toogle magit process window."
  (interactive)
  (let ((magit-process-window
         (some-window
          (lambda (window)
            (equal (window-buffer window) (get-buffer magit-process-buffer-name))))))
    (if magit-process-window
        (delete-window magit-process-window)
      (magit-display-process))))

(define-key magit-status-mode-map (kbd "$") 'magit-toggle-process-window)

(setq magit-emacsclient-executable (evm-find "emacsclient"))

(provide 'rejeep-git)
