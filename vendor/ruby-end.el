;;; ruby-end.el --- Automatic insertion of end blocks for Ruby

;; Copyright (C) 2010 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.2
;; Keywords: speed, convenience
;; URL: http://github.com/rejeep/ruby-end

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

;; ruby-end is a minor mode for Emacs that can be used with ruby-mode
;; to automatically close blocks by inserting "end" when typing a
;; block-keyword, followed by a space.
;;
;; To use ruby-end-mode, make sure that this file is in Emacs load-path:
;;   (add-to-list 'load-path "/path/to/directory/or/file")
;;
;; Then require ruby-end:
;;   (require 'ruby-end)
;;
;; ruby-end-mode is automatically started in ruby-mode.


;;; Code:

(defvar ruby-end-expand-key "SPC"
  "Space key name.")

(defvar ruby-end-mode-map
  (let ((map (make-sparse-keymap))
        (key (read-kbd-macro ruby-end-expand-key)))
    (define-key map key 'ruby-end-space)
    map)
  "Keymap for `ruby-end-mode'.")

(defconst ruby-end-expand-before-re
  "\\(?:^\\|\\s-+\\)\\(?:def\\|if\\|class\\|module\\|unless\\|case\\|while\\|do\\|until\\|for\\|begin\\)"
  "Regular expression matching before point.")

(defconst ruby-end-expand-after-re
  "\\s-*$"
  "Regular expression matching after point.")

(defun ruby-end-space ()
  "Called when SPC-key is pressed."
  (interactive)
  (cond
   ((ruby-end-expand-p)
    (ruby-end-insert-end)
    (insert " "))
   (t
    (let ((ruby-end-mode nil))
      (call-interactively
       (key-binding
        (read-kbd-macro ruby-end-expand-key)))))))

(defun ruby-end-insert-end ()
  "Closes block by inserting end."
  (let ((whites
         (save-excursion
           (back-to-indentation)
           (current-column))))
    (save-excursion
      (newline)
      (indent-line-to (+ whites ruby-indent-level))
      (newline)
      (indent-line-to whites)
      (insert "end"))))

(defun ruby-end-expand-p ()
  "Checks if expansion (insertion of end) should be done."
  (and
   (ruby-end-code-at-point-p)
   (looking-back ruby-end-expand-before-re)
   (looking-at ruby-end-expand-after-re)))

(defun ruby-end-code-at-point-p ()
  "Checks if point is code, or comment or string."
  (let ((properties (text-properties-at (point))))
    (and
     (null (memq 'font-lock-string-face properties))
     (null (memq 'font-lock-comment-face properties)))))

;;;###autoload
(define-minor-mode ruby-end-mode
  "Automatic insertion of end blocks for Ruby."
  :init-value nil
  :lighter " end"
  :keymap ruby-end-mode-map)

(add-hook 'ruby-mode-hook 'ruby-end-mode)

(provide 'ruby-end)

;;; ruby-end.el ends here
