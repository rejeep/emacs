;;; rejeep-ido - Interactively do


(ido-mode t)

(setq ido-everywhere t)
(setq ido-enable-prefix nil)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-max-prospects 10)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order '(".rb" ".el"))

(provide 'rejeep-ido)
