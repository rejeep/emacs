;; Put stuff here that don't fit anywhere else.

(defun google-region()
  "Google the selected region."
  (interactive)
  (browse-url (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
                      (buffer-substring (region-beginning) (region-end)))))

;; Open files with specified external programs.
(require 'openwith)
(openwith-mode t)

;; Dabbrev-mode like feature.
(require 'pabbrev)
(global-pabbrev-mode)
(define-key pabbrev-mode-map (kbd "M-/") 'pabbrev-expand-maybe)
(define-key pabbrev-mode-map (kbd "<tab>") 'yas/expand)


;; I/Fly-spell

;; Default dictionary.
(setq ispell-dictionary "english")

;; Add flyspell to modes.
(add-hook 'ruby-mode-hook 'flyspell-prog-mode)
(add-hook 'magit-mode-hook 'flyspell-mode)