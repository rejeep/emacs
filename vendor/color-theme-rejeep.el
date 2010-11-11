;;; color-theme-rejeep.el --- rejeep color theme

;; Copyright (C) 2010 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: colors
;; URL: http://github.com/rejeep/color-theme-rejeep

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; (require 'color-theme)
;; (require 'color-theme-rejeep)
;; (color-theme-initialize)
;; (color-theme-rejeep)

;;; Code:


(defun color-theme-rejeep ()
  "Color theme by rejeep, inspired by Twilight and Railscasts."
  (interactive)
  (color-theme-install
   '(color-theme-rejeep
     ((background-color . "#141414")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "#A7A7A7")
      (foreground-color . "#F8F8F8")
      (mouse-color . "sienna1"))
     (default ((t (:background "#141414" :foreground "#CACACA"))))
     (blue ((t (:foreground "blue"))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "#141414" :foreground "#CACACA"))))
     (font-lock-builtin-face ((t (:foreground "#CACACA"))))
     (font-lock-comment-face ((t (:foreground "#5F5A60"))))
     (font-lock-constant-face ((t (:foreground "#CF6A4C"))))
     (font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
     (font-lock-function-name-face ((t (:foreground "#CDA869"))))
     (font-lock-keyword-face ((t (:foreground "#CC7833"))))
     (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
     (font-lock-reference-face ((t (:foreground "SlateBlue"))))
     (font-lock-string-face ((t (:foreground "#8F9D6A"))))
     (font-lock-type-face ((t (:foreground "#4b6983"))))
     (font-lock-variable-name-face ((t (:foreground "#7587A6"))))
     (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "red"))))
     (minibuffer-prompt ((t (:foreground "#5F5A60"))))
     (ido-subdir ((t (:foreground "#CF6A4C"))))
     (ido-first-match ((t (:foreground "#8F9D6A"))))
     (ido-only-match ((t (:foreground "#8F9D6A"))))
     (mumamo-background-chunk-submode ((t (:background "#222222"))))
     (font-lock-warning-face ((t (:background "#EE799F" :foreground "red"))))
     (gui-element ((t (:background "#D4D0C8" :foreground "black"))))
     (region ((t (:background "#27292A"))))
     (mode-line ((t (:background "grey75" :foreground "black"))))
     (highlight ((t (:background "#111111"))))
     (highline-face ((t (:background "SeaGreen"))))
     (left-margin ((t (nil))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil))))
     (underline ((nil (:underline nil))))
     (zmacs-region ((t (:background "snow" :foreground "blue")))))))


(provide 'color-theme-rejeep)

;;; color-theme-rejeep.el ends here