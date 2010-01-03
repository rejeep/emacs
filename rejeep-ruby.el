;;; rejeep-ruby.el --- Ruby and Rails specific settings

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))

;; Ruby
(add-hook 'ruby-mode-hook 'flyspell-prog-mode)
(add-hook 'ruby-mode-hook 'wrap-region-mode)
(add-hook 'ruby-mode-hook
          '(lambda()
             (define-key ruby-mode-map (kbd "C-M-h") 'backward-kill-word)))

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

(provide 'rejeep-ruby)
