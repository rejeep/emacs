;;; enclose.el --- Enclose cursor within punctuation pairs

;; Copyright (C) 2010 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.2
;; Keywords: speed, convenience
;; URL: http://github.com/rejeep/enclose

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

;; Enclose is a minor mode that encloses cursor within punctuation
;; pairs. For example, hitting the key "(" will insert "(" and ")" and
;; place the cursor in between.

;; To use Enclose mode, make sure that this file is in Emacs load-path:
;;   (add-to-list 'load-path "/path/to/directory/or/file")
;;
;; Then require enclose:
;;   (require 'enclose)

;; To start enclose mode:
;;   (enclose-mode t) or M-x enclose-mode
;;
;; If you only want enclose mode active in some modes, use hooks:
;;   (add-hook 'ruby-mode-hook 'enclose-mode)
;;
;; Or if you want to activate it in all buffers, use the global mode:
;;   (enclose-global-mode t)

;; When enclose mode is active, pressing any key, that is a key, in the
;; `enclose-table' hash, will insert the pair and place the cursor in
;; between. At this point the cursor is in focus, meaning that
;; pressing DEL would remove both punctuations around the cursor and
;; pressing the closing key will jump over the right punctuation.
;; Moving the cursor when in focus, unfocus the cursor.

;; Hitting the DEL key in focus removes both punctuations around the
;; cursor, when `enclose-remove-pair' is set to t, which is
;; default. When this variable is nil, only the left punctuation is
;; removed.

;; Keys that encloses cursor are defined in `enclose-table'. You can
;; add and remove new triggers by using the functions
;; `enclose-add-encloser' and `enclose-remove-encloser' respectively.
;;   (enclose-add-encloser "`" "`")
;;   (enclose-remove-encloser "(")

;; Some modes may have conflicting key bindings with enclose. To
;; avoid conflicts, the list `enclose-except-modes' contains names
;; of modes where enclose should not be activated (note, only in
;; the global mode). You can add new modes like this:
;;   (add-to-list 'enclose-except-modes 'conflicting-mode)


;;; Code:

(defvar enclose-table
  (let ((table (make-hash-table :test 'equal)))
    (puthash "\"" "\"" table)
    (puthash "'"  "'"  table)
    (puthash "("  ")"  table)
    (puthash "{"  "}"  table)
    (puthash "["  "]"  table)
    table)
  "Table with encloser pairs.")

(defvar enclose-mode-map (make-sparse-keymap)
  "Keymap for `enclose-mode'.")

(defvar enclose-focus nil
  "If cursor is in focus or not.")
(make-variable-buffer-local 'enclose-focus)

(defvar enclose-last-pos 0
  "Last position an enclose command was executed at.")
(make-variable-buffer-local 'enclose-last-pos)

(defvar enclose-remove-pair t
  "Decides if pair should be removed, or just the left one.")

(defvar enclose-except-modes '()
  "A list of modes in which `enclose-mode' should not be activated.")

(defconst enclose-del-key "DEL"
  "Delete key.")

(defconst enclose-anti-regex "[a-zA-Z0-9]+"
  "Enclosing functionality should not be activated when surrounded by,
or before text matching this regex.")


(defmacro enclose-command (&rest body)
  "Executes BODY and then updates `enclose-last-pos' accordingly."
  `(progn
     ,@body
     (setq enclose-last-pos (point))))


(defun enclose-trigger (key)
  "Called when trigger key (any key or value in `enclose-table') is hit."
  (if (enclose-jump-p key)
      (enclose-jump)
    (enclose-insert key)))

(defun enclose-jump ()
  "Jump the cursor."
  (enclose-command
   (forward-char 1)
   (enclose-unfocus)))

(defun enclose-jump-p (key)
  "Checks if cursor should jump."
  (and
   enclose-focus
   (let ((value (gethash key enclose-table)))
     (if value (equal key value) t))
   (if (char-before)
       (equal (gethash (char-to-string (char-before)) enclose-table) key))))

(defun enclose-insert (left)
  "Inserts LEFT and right buddy or falls back."
  (if (enclose-insert-pair-p left)
      (let ((right (gethash left enclose-table)))
        (enclose-insert-pair left right))
    (enclose-insert-fallback left)))

(defun enclose-insert-pair (left right)
  "Insert LEFT and RIGHT and place cursor between."
  (enclose-command
   (insert left right)
   (backward-char 1)
   (enclose-focus)))

(defun enclose-insert-fallback (left)
  "Falls back and calls function LEFT was bound to before
`enclose-mode'."
  (enclose-command
   (enclose-fallback left)
   (enclose-unfocus)))

(defun enclose-insert-pair-p (key)
  "Checks if insertion should be a pair or not."
  (unless (region-active-p)
    (and
     (gethash key enclose-table)
     (not
      (looking-at enclose-anti-regex)))))

(defun enclose-remove ()
  "Called when user hits the key `enclose-del-key'."
  (interactive)
  (if (enclose-remove-pairing-p)
      (enclose-remove-pair)
    (enclose-remove-fallback)))

(defun enclose-remove-pair ()
  "Remove pair around cursor."
  (enclose-command
   (delete-region (- (point) 1) (+ (point) 1))
   (enclose-focus)))

(defun enclose-remove-fallback ()
    "Falls back and calls function `enclose-del-key' was bound to
before `enclose-mode'."
  (enclose-command
   (enclose-fallback enclose-del-key)
   (enclose-unfocus)))

(defun enclose-remove-pairing-p ()
  "Checks if removing should be on pair or not."
  (and
   enclose-remove-pair
   enclose-focus
   (not (or (bobp) (eobp)))
   (let ((before (char-to-string (char-before)))
         (after (char-to-string (char-after))))
     (equal (gethash before enclose-table) after))))

(defun enclose-add-encloser (left right)
  "Add LEFT and RIGHT as an encloser pair."
  (puthash left right enclose-table)
  (enclose-define-trigger left))

(defun enclose-remove-encloser (left)
  "Remove LEFT as an encloser trigger."
  (remhash left enclose-table)
  (enclose-unset-key left))

(defun enclose-fallback (key)
  "Executes function that KEY was bound to before `enclose-mode'."
  (let ((enclose-mode nil))
    (call-interactively
     (key-binding
      (read-kbd-macro key)))))

(defun enclose-define-keys ()
  "Defines key bindings for `enclose-mode'."
  (enclose-define-key enclose-del-key 'enclose-remove)
  (maphash
   (lambda (left right)
     (enclose-define-trigger left)
     (enclose-define-trigger right))
   enclose-table))

(defun enclose-define-trigger (key)
  "Defines KEY as trigger."
  (enclose-define-key
   key
   `(lambda ()
      (interactive)
      (enclose-trigger ,key))))

(defun enclose-unset-key (key)
  "Remove KEY as an encloser trigger."
  (enclose-define-key key nil))

(defun enclose-define-key (key fn)
  "Binds KEY to FN in `enclose-mode-map'."
  (define-key enclose-mode-map (read-kbd-macro key) fn))

(defun enclose-focus ()
  (setq enclose-focus t))

(defun enclose-unfocus ()
  (setq enclose-focus nil))

(defun enclose-post-command ()
  "Unfocus if cursor has moved."
  (if (/= enclose-last-pos (point))
      (enclose-unfocus)))


;;;###autoload
(define-minor-mode enclose-mode
  "Enclose cursor within punctuation pairs."
  :init-value nil
  :lighter " enc"
  :keymap enclose-mode-map
  (cond (enclose-mode
         (enclose-define-keys)
         (add-hook 'post-command-hook 'enclose-post-command))
        (t
         (remove-hook 'post-command-hook 'enclose-post-command))))

;;;###autoload
(defun turn-on-enclose-mode ()
  "Turn on `enclose-mode'"
  (interactive)
  (unless (member major-mode enclose-except-modes)
    (enclose-mode +1)))

;;;###autoload
(defun turn-off-enclose-mode ()
  "Turn off `enclose-mode'"
  (interactive)
  (enclose-mode -1))

;;;###autoload
(define-globalized-minor-mode enclose-global-mode
  enclose-mode
  turn-on-enclose-mode)


(provide 'enclose)

;;; enclose.el ends here