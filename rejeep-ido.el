;;; rejeep-ido.el --- Eveything ido related

;; Interactively do, or ido
(ido-mode t)

;; Prefix matching enabled.
(setq ido-enable-prefix nil)

;; Flexible string matching (pacu will match package.use).
(setq ido-enable-flex-matching t)

;; Always create buffer if no buffer matches search string.
(setq ido-create-new-buffer 'always)

;; Look for filename at point.
(setq ido-use-filename-at-point nil)

;; Show max 10 matches.
(setq ido-max-prospects 10)

(provide 'rejeep-ido)