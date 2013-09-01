;;; rejeep-ido - Interactively do


(ido-mode t)

(setq ido-everywhere t)
(setq ido-enable-prefix nil)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-max-prospects 10)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order '(".rb" ".el" ".js"))
(setq ido-case-fold t)

(autoload 'ido-at-point-mode "ido-at-point")
(ido-at-point-mode)

(provide 'rejeep-ido)
