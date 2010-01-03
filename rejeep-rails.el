;;; rejeep-rails.el --- Rails specific settings

;; Rinari
(require 'rinari)
(add-hook 'rinari-minor-mode-hook
          '(lambda()
             ;; No # -*- coding: utf-8 -*- commets.
             (setq ruby-insert-encoding-magic-comment nil)

             ;; TAGS file is in rails root if any.
             (setq rinari-tags-file-name "TAGS")))

;; Rhtml
(autoload 'rhtml-mode "rhtml-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb$" . rhtml-mode))

(add-hook 'rhtml-mode-hook
          '(lambda()
             (custom-set-faces
              '(erb-comment-delim-face ((t (:foreground "#5F5A60" :background "grey15"))))
              '(erb-comment-face ((t (:italic t :foreground "#5F5A60" :background "grey15"))))
              '(erb-delim-face ((t (:foreground "#FFAEEA" :background "grey15"))))
              '(erb-face ((t (:background "grey15"))))
              '(erb-out-delim-face ((t (:foreground "#FFAEEA" :background "grey15"))))
              '(erb-out-face ((t (:background "grey15")))))

             (setq wrap-region-tag-active t)
             (wrap-region-mode t)))

(provide 'rejeep-rails)
