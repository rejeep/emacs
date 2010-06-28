;;; keats-interactive.el --- Interactive mode for Keats mode

;; Copyright (C) 2009 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience, help
;; URL: http://github.com/rejeep/keats
;; Package-Requires: ((keats-mode "1.0.0") (easy-menu))

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

;; This mode is an extension to `keats-mode'. This mode adds extends
;; Keats mode with an interactive interface.

;; To using Keats Interactive mode, make sure that this file is in
;; Emacs load-path
;; (add-to-list 'load-path "/path/to/directory/or/file")
;;
;; Then require Keats
;; (require 'keats-interactive)

;; To start Keats
;; (keats-interactive-mode t) or M-x keats-interactive-mode

;; You are most likely to use this mode if you want to add, edit or
;; remove many keats at the same time. In this mode these keybindings
;; are set:
;;  * a - Adds a new keat
;;  * e - Edits keat at point
;;  * r - Removes keat at point
;;  * n - Move one step down
;;  * p - Move one step up
;;  * q - Quit the buffer and mode
;;  * w - Writes keats to file
;;  * s - Search for keyword
;;  * RET - Runs command for which key at point is connected to. With
;;    prefix argument (C-u) the interactive buffer is closed before
;;    the command is executed.

;;; Code:

(require 'keats)
(require 'easymenu)

(defconst keats-interactive-temp-buffer "*Keats Interactive*"
  "Buffer used for this mode.")

(defvar keats-interactive-title-height nil
  "The number of rows that the title is high.")

(defvar keats-interactive-mode-hook '()
  "Hook for this mode. Is evaluated last in mode startup.")

(defvar keats-interactive-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'keats-interactive-add)
    (define-key map (kbd "e") 'keats-interactive-edit)
    (define-key map (kbd "r") 'keats-interactive-remove)
    (define-key map (kbd "n") 'keats-interactive-next)
    (define-key map (kbd "C-n") 'keats-interactive-next)
    (define-key map (kbd "<down>") 'keats-interactive-next)
    (define-key map (kbd "p") 'keats-interactive-previous)
    (define-key map (kbd "C-p") 'keats-interactive-previous)
    (define-key map (kbd "<up>") 'keats-interactive-previous)
    (define-key map (kbd "q") 'keats-interactive-quit)
    (define-key map (kbd "w") 'keats-interactive-write)
    (define-key map (kbd "s") 'keats-interactive-search)
    (define-key map (kbd "RET") 'keats-interactive-run)
    map)
  "Keymap for `keats-interactive-mode'.")

(defface keats-interactive-title
  '((((class color) (background dark))
     :foreground "red"
     :bold t))
  "Face for title."
  :group 'keats)

(defface keats-interactive-highlight
  '((((class color) (background light))
     :background "gray95")
    (((class color) (background dark))
     :background "dim gray"))
  "Face for active line."
  :group 'keats)

(easy-menu-define keats-interactive-mode-menu keats-interactive-mode-map
  "Keats menu"
  '("Keats"
    ["Add" keats-interactive-add t]
    ["Edit" keats-interactive-edit t]
    ["Remove" keats-interactive-remove t]
    ["Next" keats-interactive-next t]
    ["Previous" keats-interactive-previous t]
    ["Quit" keats-interactive-quit t]
    ["Write" keats-interactive-write t]
    ["Search" keats-interactive-search t]
    ["Run" keats-interactive-run t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun keats-interactive-add ()
  "Adds a new keat if it does not already exists."
  (interactive)
  (let* ((key (keats-key nil))
         (keat (keats-key-exists key)))
    (if (and key keat)
        (message "%s already exist. Edit instead." key)
      (let ((keat (keats-add key)))
        (if keat
            (keats-interactive-insert-keat keat))))))

(defun keats-interactive-edit ()
  "Edits keat at point."
  (interactive)
  (let* ((key (keats-interactive-key-at-point))
         (keat (keats-key-exists key)))
    (when (and keat (keats-edit key))
      (keats-interactive-remove-keat)
      (keats-interactive-insert-keat keat (line-beginning-position)))))

(defun keats-interactive-remove ()
  "Removes keat at point."
  (interactive)
  (if (keats-remove (keats-interactive-key-at-point))
      (keats-interactive-remove-keat t)))

(defun keats-interactive-next ()
  "Moves one step down in the list of keats."
  (interactive)
  (keats-interactive-move
   (lambda ()
     (if (< (line-number-at-pos nil) (count-lines (point-min) (point-max)))
         (next-line)))))

(defun keats-interactive-previous ()
  "Moves one step down in the list of keats."
  (interactive)
  (keats-interactive-move
   (lambda ()
     (if (> (line-number-at-pos nil) (1+ keats-interactive-title-height))
         (previous-line)))))

(defun keats-interactive-quit ()
  "Exits mode by closing buffer."
  (interactive)
  (kill-this-buffer))

(defun keats-interactive-write ()
  "Writes keats to file."
  (interactive)
  (keats-write))

(defun keats-interactive-search ()
  "Shortcut to `keats-search'."
  (interactive)
  (let ((inhibit-read-only t))
    (keats-search (read-string "Query: "))))

(defun keats-interactive-run (arg)
  "Runs command for which key at point is connected to.
With prefix argument, kill interactive buffer before executing command."
  (interactive "P")
  (let* ((key (keats-interactive-key-at-point))
         (function (key-binding (read-kbd-macro key))))
    (cond (function
           (if arg
               (kill-this-buffer))
           (call-interactively function))
          (t
           (message "%s runs no command" key)))))

(defun keats-interactive-move (function)
  "Helper for moving up and down in list. Makes sure that correct
lines are highlighted."
  (goto-char (line-beginning-position))
  (keats-interactive-put-text-property 'face nil)
  (funcall function)
  (keats-interactive-put-text-property 'face 'keats-interactive-highlight))

(defun keats-interactive-insert-keat (keat &optional pos)
  "Inserts a keat at POS or if POS is nil last in the list."
  (save-excursion
    (goto-char (or pos (point-max)))
    (let ((inhibit-read-only t))
      (unless (= (current-column) 0)
        (insert "\n"))
      (insert (keats-to-string keat))))
  (keats-interactive-put-text-property 'face 'keats-interactive-highlight))

(defun keats-interactive-insert-keats (keats &optional pos)
  "Inserts a list of keats. See `keats-interactive-insert-keat'."
  (dolist (keat keats)
    (keats-interactive-insert-keat keat pos)))

(defun keats-interactive-remove-keat (&optional whole-line)
  "Removes keat at point from list (not from memory)."
  (let ((inhibit-read-only t))
    (delete-region (line-beginning-position) (line-end-position))
    (if whole-line
        (cond ((and (eobp) (> (line-number-at-pos nil) (+ keats-interactive-title-height 1)))
               (backward-delete-char 1)
               (keats-interactive-put-text-property 'face 'keats-interactive-highlight)
               (goto-char (line-beginning-position)))
              (t
               (delete-char 1)
               (keats-interactive-previous))))))

(defun keats-interactive-key-at-point ()
  "Returns key at point."
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (string-match (concat "^\\(.*\\)" keats-to-string-delimiter) line)
    (match-string-no-properties 1 line)))

(defun keats-interactive-put-text-property (prop val &optional beg end)
  "Changes the face of the current line or from BEG to END if non
nil."
  (let ((inhibit-read-only t))
    (put-text-property (or beg (line-beginning-position)) (or end (line-end-position)) prop val)))

(defun keats-interactive-set-title (title)
  "Sets the title."
  (goto-char (point-min))
  (let ((inhibit-read-only t) (min) (max))
    (delete-region (line-beginning-position) (line-end-position))
    (insert title)
    (setq min (point-min))
    (setq max (point-max))
    (setq keats-interactive-title-height (count-lines min max))
    (keats-interactive-put-text-property 'face 'keats-interactive-title min max)
    (newline)))

;;;###autoload
(defun keats-interactive-mode (title)
  "Major mode to interactively manage Keats."
  (switch-to-buffer (get-buffer-create keats-interactive-temp-buffer))
  (delete-region (point-min) (point-max))
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (use-local-map keats-interactive-mode-map)
  (setq mode-name "Keats Interactive")
  (setq major-mode 'keats-interactive-mode)
  (keats-interactive-set-title title)
  (run-mode-hooks 'keats-interactive-mode-hook))

(provide 'keats-interactive)

;;; keats.el ends here
