;;; rejeep-ido - Interactively do


(ido-mode t)

(setq
 ido-everywhere t
 ido-enable-prefix nil
 ido-enable-flex-matching t
 ido-create-new-buffer 'always
 ido-max-prospects 10
 ido-create-new-buffer 'always
 ido-file-extensions-order '(".rb" ".el"))

(provide 'rejeep-ido)
