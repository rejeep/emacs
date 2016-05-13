;;; init.el - Fragor Maximus -*- lexical-binding: t; -*-

(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(require 's)
(require 'f)
(require 'ht)
(require 'git)
(require 'ert)
(require 'use-package)

(setq default-directory (f-full (getenv "HOME")))

(defun load-local (file)
  (load (f-expand file user-emacs-directory)))

(load-local "defuns")
(load-local "misc")
(when (eq system-type 'darwin)
  (load-local "osx"))

(load-theme 'dracula :no-confirm)

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (string= (buffer-name) "*scratch*")
              (animate-string ";; Yo moma is a dirty skank!" (/ (frame-height) 2)))))


;;;; Packages

(use-package hl-line
  :config (set-face-background 'hl-line "#073642"))

(use-package dash
  :config (dash-enable-font-lock))

(use-package dired-x)

(use-package ido
  :init (progn (ido-mode 1)
               (ido-everywhere 1))
  :config
  (progn
    (setq ido-case-fold t)
    (setq ido-everywhere t)
    (setq ido-enable-prefix nil)
    (setq ido-enable-flex-matching t)
    (setq ido-create-new-buffer 'always)
    (setq ido-max-prospects 10)
    (setq ido-use-faces nil)
    (setq ido-file-extensions-order '(".rb" ".el" ".coffee" ".js"))
    (add-to-list 'ido-ignore-files "\\.DS_Store")
    (add-to-list 'ido-ignore-files "appspec.yml")))

(use-package nyan-mode
  :init (nyan-mode 1))

(use-package smex
  :init (smex-initialize)
  :bind ("M-x" . smex))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package popwin
  :config (popwin-mode 1))

(use-package projectile
  :init (projectile-global-mode 1)
  :config
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-require-project-root nil)
    (setq projectile-completion-system 'ido)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")))

(use-package drag-stuff
  :init (drag-stuff-global-mode 1)
  :bind (("M-N" . drag-stuff-down)
         ("M-P" . drag-stuff-up)))

(use-package misc
  :bind ("M-z" . zap-up-to-char))

(use-package magit
  :init
  (progn
    (use-package magit-blame)
    (bind-key "C-c C-a" 'magit-just-amend magit-mode-map)
    (use-package magit-gh-pulls
      :config (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)))
  :config
  (progn
    (setq magit-completing-read-function 'magit-ido-completing-read)
    (setq magit-branch-arguments nil))
  :bind ("C-x g" . magit-status))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package cua-base
  :init (cua-mode 1)
  :config
  (progn
    (setq cua-enable-cua-keys nil)
    (setq cua-toggle-set-mark nil)))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package saveplace
  :config (setq-default save-place t))

(use-package diff-hl
  :init (global-diff-hl-mode)
  :config (add-hook 'vc-checkin-hook 'diff-hl-update))

(use-package windmove
  :config (windmove-default-keybindings 'shift))

(use-package ruby-mode
  :init
  (progn
    (use-package rvm
      :init (rvm-use-default)
      :config (setq rvm-verbose nil))
    (use-package ruby-tools)
    (use-package rhtml-mode
      :mode (("\\.rhtml$" . rhtml-mode)
             ("\\.html\\.erb$" . rhtml-mode)))
    (use-package projectile-rails
      :init (add-hook 'projectile-mode-hook 'projectile-rails-on))
    (use-package bundler)
    (use-package rspec-mode
      :config
      (progn

        (add-hook 'compilation-mode-hook
                  (lambda ()
                    (when (eq major-mode 'rspec-compilation-mode)
                      (setq compilation-scroll-output t)
                      (local-set-key (kbd "g") (lambda ()
                                                 (interactive)
                                                 (rspec-rerun))))))

        (setq rspec-use-rvm t)
        (setq rspec-use-rake-when-possible nil)
        (defadvice rspec-compile (around rspec-compile-around activate)
          "Use BASH shell for running the specs because of ZSH issues."
          (let ((shell-file-name "/bin/bash"))
            ad-do-it)))))
  :config
  (progn
    (setq ruby-align-to-stmt-keywords '(begin if while unless until case for def))
    (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)
    (setq ruby-deep-indent-paren nil))
  :bind (("C-M-h" . backward-kill-word)
         ("C-M-n" . scroll-up-five)
         ("C-M-p" . scroll-down-five))
  :mode (("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode)))

(use-package markdown-mode
  :config
  (progn
    (bind-key "M-n" 'open-line-below markdown-mode-map)
    (bind-key "M-p" 'open-line-above markdown-mode-map))
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)))

(use-package smartparens
  :init
  (progn
    (use-package smartparens-config)
    (use-package smartparens-ruby)
    (use-package smartparens-html)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1))
  :config
  (progn
    (setq smartparens-strict-mode t)
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p)))
  :bind
  (("C-M-k" . sp-kill-sexp-with-a-twist-of-lime)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-n" . sp-up-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-p" . sp-backward-down-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("M-s" . sp-splice-sexp)
   ("M-r" . sp-splice-sexp-killing-around)
   ("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-(" . sp-backward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)
   ("M-S" . sp-split-sexp)
   ("M-J" . sp-join-sexp)
   ("C-M-t" . sp-transpose-sexp)))

(use-package flycheck
  :config
  (progn
    (setq flycheck-display-errors-function nil)
    (add-hook 'after-init-hook 'global-flycheck-mode)))

(use-package flycheck-cask
  :init (add-hook 'flycheck-mode-hook 'flycheck-cask-setup))

(use-package yasnippet
  :init
  (progn
    (add-hook 'after-save-hook
              (lambda ()
                (when (eql major-mode 'snippet-mode)
                  (yas-reload-all))))
    (setq yas-snippet-dirs (list (f-expand "snippets" user-emacs-directory)))
    (setq yas-prompt-functions '(yas/ido-prompt))
    (yas-global-mode 1))
  :mode ("\\.yasnippet" . snippet-mode))

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package feature-mode
  :mode ("\\.feature$" . feature-mode)
  :config
  (add-hook 'feature-mode-hook
            (lambda ()
              (electric-indent-mode -1))))

(use-package cc-mode
  :config
  (progn
    (add-hook 'c-mode-hook (lambda () (c-set-style "bsd")))
    (add-hook 'java-mode-hook (lambda () (c-set-style "bsd")))
    (setq tab-width 2)
    (setq c-basic-offset 2)))

(use-package css-mode
  :config (setq css-indent-offset 2))

(use-package js-mode
  :mode ("\\.json$" . js-mode)
  :init
  (progn
    (add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("Jakefile$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :bind (("C-a" . back-to-indentation-or-beginning-of-line)
         ("C-M-h" . backward-kill-word))
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
    (add-hook 'js2-mode-hook (lambda ()
                               (bind-key "M-j" 'join-line-or-lines-in-region js2-mode-map)))))

(use-package coffee-mode
  :config
  (progn
    (add-hook 'coffee-mode-hook
              (lambda ()
                (bind-key "C-j" 'coffee-newline-and-indent coffee-mode-map)
                (bind-key "C-M-h" 'backward-kill-word coffee-mode-map)
                (setq coffee-tab-width 2)))))

(use-package nvm)

(use-package sh-script
  :config (setq sh-basic-offset 2))

(use-package emacs-lisp-mode
  :init
  (progn
    (use-package eldoc
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
    (use-package macrostep
      :bind ("C-c e" . macrostep-expand))
    (use-package ert
      :config (add-to-list 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords)))
  :config
  (progn
    (setq tab-always-indent 'complete)
    (add-to-list 'completion-styles 'initials t))
  :bind (("M-." . find-function-at-point)
         ("M-&" . complete-symbol))
  :interpreter (("emacs" . emacs-lisp-mode))
  :mode ("Cask" . emacs-lisp-mode))

(use-package html-script-src)

(use-package haml-mode)
(use-package sass-mode)

(use-package eshell
  :bind ("M-e" . eshell)
  :init
  (add-hook 'eshell-first-time-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "htop")))
  :config
  (progn
    (setq eshell-history-size 5000)
    (setq eshell-save-history-on-exit t)))

(use-package flx-ido
  :init (flx-ido-mode 1))

(use-package ido-vertical-mode
  :init (ido-vertical-mode 1))

(use-package web-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
  :config (progn
            (add-hook 'web-mode-hook
                      (lambda ()
                        (setq web-mode-enable-css-colorization t)
                        (setq web-mode-markup-indent-offset 2)
                        (setq web-mode-style-padding 2)
                        (setq web-mode-script-padding 2)))))

(use-package prodigy
  :init (progn
          (add-hook 'prodigy-mode-hook
                    (lambda ()
                      (setq-local show-trailing-whitespace nil))))
  :demand t
  :bind ("C-x p" . prodigy))

(use-package discover
  :init (global-discover-mode 1))

(use-package ert-async
  :config (progn
            (remove-hook 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords)
            (add-hook 'emacs-lisp-mode-hook 'ert-async-activate-font-lock-keywords)))

(use-package ibuffer
  :config (setq ibuffer-expert t)
  :bind ("C-x C-b" . ibuffer))

(use-package cl-lib-highlight
  :init (cl-lib-highlight-initialize))

(use-package idomenu
  :bind ("M-i" . idomenu))

(use-package httprepl)

(use-package cask-mode)

(use-package ag)

(use-package git-gutter
  :init (global-git-gutter-mode +1))

(use-package ibuffer-projectile
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-projectile-set-filter-groups)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package overseer)

(use-package smart-mode-line
  :init
  (progn
    (setq sml/no-confirm-load-theme t)
    (sml/setup)))

(use-package visible-mark
  :init (global-visible-mark-mode 1))

(use-package beacon
  :init (beacon-mode 1))

(use-package avy
  :bind (("C-\\" . avy-goto-char)
         ("M-g" . avy-goto-line))
  :config (avy-setup-default))


;;;; Bindings

(bind-key "C-a" 'back-to-indentation-or-beginning-of-line)
(bind-key "C-7" 'comment-or-uncomment-current-line-or-region)
(bind-key "C-6" 'linum-mode)
(bind-key "C-v" 'scroll-up-five)
(bind-key "C-j" 'newline-and-indent)

(bind-key "M-n" 'open-line-below)
(bind-key "M-p" 'open-line-above)
(bind-key "M-+" 'text-scale-increase)
(bind-key "M-_" 'text-scale-decrease)
(bind-key "M-j" 'join-line-or-lines-in-region)
(bind-key "M-v" 'scroll-down-five)
(bind-key "M-k" 'kill-this-buffer)
(bind-key "M-o" 'other-window)
(bind-key "M-1" 'delete-other-windows)
(bind-key "M-2" 'split-window-below)
(bind-key "M-3" 'split-window-right)
(bind-key "M-0" 'delete-window)
(bind-key "M-}" 'next-buffer)
(bind-key "M-{" 'previous-buffer)
(bind-key "M-`" 'other-frame)
(bind-key "M-w" 'kill-region-or-thing-at-point)

(bind-key "C-c g" 'google)
(bind-key "C-c d" 'duplicate-current-line-or-region)
(bind-key "C-c n" 'clean-up-buffer-or-region)
(bind-key "C-c s" 'swap-windows)
(bind-key "C-c r" 'rename-this-buffer-and-file)
(bind-key "C-c k" 'delete-this-buffer-and-file)

(bind-key "C-M-h" 'backward-kill-word)
(bind-key "C-c C-n" 'todo)

(bind-key
 "C-x C-c"
 (lambda ()
   (interactive)
   (if (y-or-n-p "Quit Emacs? ")
       (save-buffers-kill-emacs))))

(bind-key
 "C-8"
 (lambda ()
   (interactive)
   (find-file user-init-file)))


;;;; Sandbox

(let ((sandbox-path (f-expand "sandbox" user-emacs-directory)))
  (when (f-dir? sandbox-path)
    (-each (f--files sandbox-path (f-ext? it "el")) 'load)))
