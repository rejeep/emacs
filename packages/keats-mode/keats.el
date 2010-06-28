;;; keats.el --- Keybinding Cheats

;; Copyright (C) 2009 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience, help
;; URL: http://github.com/rejeep/keats

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

;; Keats is a mode for storing notes about Emacs key bindings so you
;; easier remember them.

;; To use Keats, make sure that this file is in Emacs load-path
;; (add-to-list 'load-path "/path/to/directory/or/file")
;;
;; Then require Keats
;; (require 'keats)

;; To start Keats
;; (keats-mode t) or M-x keats-mode

;; Keats stores all keats in the file given by the variable
;; `keats-file'. Change to whatever suits you.
;; (setq keats-file "~/emacs.d/keats")

;; When you add, edit and delete keats Keats will automatically save
;; the keats to `keats-file'. The number of changes before save is
;; determined by `keats-save-at'. Set to nil if no auto saving should
;; be done.

;; Many of the Keats commands will prompt for a key sequence. To enter
;; one, start type the sequence and when done press RET. If you want
;; to abort, press C-g. To type a key by hand, give prefix key
;; (C-u). In this prompt the key sequence can be typed by hand. This
;; is useful if the key contains "C-g" or "RET".
;;
;; == ADD (C-c k a)
;; Will add a new keat if it does not already exist. If it does exists
;; the edit action will be called with the same key. Read below under
;; edit.
;;
;; == EDIT (C-c k e)
;; Edits an already existing keat.
;;
;; == DESCRIPTION (C-c k d)
;; Prints the description for a key sequence.
;;
;; == REMOVE (C-c k r)
;; Removes a keat.
;;
;; == WRITE (C-c k w)
;; Writes `keats-list' to file. This is done every time Emacs is killed.
;;
;; == SEARCH (C-c k s)
;; Searches (without respect to case) in keat descriptions for a given
;; regexp. If there's at least one hit, `keats-interactive-mode' is
;; started showing all matching keats.
;;
;; == INTERACTIVE (C-c k i)
;; Opens `keats-interactive-mode'.

;;; Code:

(defconst keats-temp-buffer "*Keats*"
  "Temp buffer.")

(defvar keats-mode-map (make-sparse-keymap)
  "Keymap for `keats-mode'.")

(define-prefix-command 'keats-mode-map)
(global-set-key (kbd "C-c k") 'keats-mode-map)
(let ((map keats-mode-map))
  (define-key map (kbd "a") 'keats-add)
  (define-key map (kbd "e") 'keats-edit)
  (define-key map (kbd "r") 'keats-remove)
  (define-key map (kbd "d") 'keats-print-description)
  (define-key map (kbd "s") 'keats-search)
  (define-key map (kbd "w") 'keats-write)
  (define-key map (kbd "i") 'keats-interactive))

(defvar keats-list '()
  "List of plists where each plist is a keat on the form (:key
\"key\" :description \"description\")")

(defvar keats-save-count 0
  "Holds the value for how many changes there has been since last
  save.")

(defcustom keats-file "~/.keats"
  "Path to file where keats are stored."
  :group 'keats)

(defcustom keats-save-at 5
  "Tells how many changes (add, edit and remove) there can be
without auto saving. nil value means no auto saving."
  :group 'keats)

(defcustom keats-to-string-delimiter ": "
  "Delimiter to muse when printing a key and description."
  :group 'keats)

(defun keats-add (&optional key description)
  "Adds a keat if it does not already exist. If it exists,
`keats-edit' is called for key if user confirms."
  (interactive)
  (setq key (keats-key key))
  (when key
    (if (keats-key-exists key)
        (if (yes-or-no-p (concat key " already exists. Do you want to edit it? "))
            (keats-edit key))
      (setq description (or description (read-string "Description: ")))
      (when description
        (let ((keat (keats-add-to-list key description)))
          (keats-update-save)
          (message "%s added" key)
          keat)))))

(defun keats-edit (&optional key description)
  "Edit the description of an already existing keat."
  (interactive)
  (setq key (keats-key key))
  (if key
      (let ((keat (keats-key-exists key)))
        (cond (keat
               (setq description (or description (read-string "Description: " (plist-get keat :description))))
               (when description
                 (plist-put keat :description description)
                 (keats-update-save)
                 (message "%s updated" key)
                 keat))
              (t
               (message "%s not found" key)
               nil)))))

(defun keats-remove (&optional key)
  "Removes a keat from the list."
  (interactive)
  (setq key (keats-key key))
  (if key
      (let ((keat (keats-key-exists key)))
        (cond (keat
               (when (yes-or-no-p (concat "Are you sure you want to remove " key "? "))
                 (setq keats-list (remove keat keats-list))
                 (keats-update-save)
                 (message "%s removed" key)
                 keat))
              (t
               (message "%s not found" key)
               nil)))))

(defun keats-print-description (&optional key)
  "Prints the description of the given key sequence."
  (interactive)
  (setq key (keats-key key))
  (if key
      (let ((keat (keats-key-exists key)))
        (if keat
            (message (plist-get keat :description))
          (message "%s not found" key)))))

(defun keats-search (query)
  "Searches for keats that matches QUERY as description."
  (interactive "*sQuery: ")
  (let ((matches '()))
    (dolist (keat keats-list)
      (if (string-match query (plist-get keat :description))
          (add-to-list 'matches keat)))
    (cond (matches
           (keats-interactive-mode (concat "Matches for: '" query "'"))
           (keats-interactive-insert-keats matches))
          (t
           (message "No matches for %s" query)))))

(defun keats-interactive ()
  "Enters keats interactive mode with all keats."
  (interactive)
  (keats-interactive-mode "Showing all keats")
  (keats-interactive-insert-keats keats-list))

(defun keats-write ()
  "Writes `keats-list' to `keats-file'."
  (interactive)
  (with-temp-file (expand-file-name keats-file)
    (insert (pp-to-string keats-list)))
  (setq keats-save-count 0))

(defun keats-read ()
  "Reads `keats-list' from `keats-file'."
  (interactive)
  (with-temp-buffer
    (insert-file-contents keats-file)
    (setq keats-list (read (current-buffer)))))

(defun keats-read-key (arg)
  "Reads a key sequence from the keyboard. To end input, press
RET and key sequence will be returned. And to abort, press C-g
and nil will be returned.

With prefix argument, type key sequence in as characters."
  (interactive "P")
  (if arg
      (keats-completing-read "Type key: " (keats-keys))
    (let ((cursor-in-echo-area t) (key) (res))
      (setq key (read-key-sequence-vector "Describe key: "))
      (while (not (string-match "RET\\|C-g" (key-description key)))
        (setq res (vconcat res key))
        (setq key (read-key-sequence-vector (key-description res))))
      (if (string= (key-description key) "RET")
          (unless (string= (key-description res) "")
            (key-description res))))))

(defun keats-key (key)
  "Will return KEY if non nil. Otherwise it will return a key
read from the keyboard."
  (or key (call-interactively 'keats-read-key)))

(defun keats-completing-read (prompt collection)
  "Like completing read, but allows spaces even if there's no
match."
  (let ((map (copy-keymap minibuffer-local-completion-map)))
    (define-key map (kbd "SPC") 'self-insert-command)
    (completing-read prompt collection)))

(defun keats-keys ()
  "Returns a list of all keys in `keats-list'."
  (mapcar (lambda (keat)
            (plist-get keat :key))
          keats-list))

(defun keats-add-to-list (key description)
  "Adds a keat to the list of keats if the key does not already
exits. If it does exists, the description is updated."
  (let ((keat `(:key ,key :description ,description)))
    (add-to-list 'keats-list keat  t)
    keat))

(defun keats-to-string (keat)
  "Returns a string representation of a keat."
  (concat (plist-get keat :key) keats-to-string-delimiter (plist-get keat :description)))

(defun keats-file-exists-p ()
  "Returns true if keats file exists. False otherwise."
  (and (file-exists-p keats-file) (not (file-directory-p keats-file))))

(defun keats-file-valid-p ()
  "Returns true if keats file is valid (read and writable). False otherwise."
  (and (file-readable-p keats-file) (file-writable-p keats-file)))

(defun keats-key-exists (key)
  "Returns t if key exists. False otherwise."
  (if key
      (let ((list keats-list))
        (while (and (not (string= (plist-get (car list) :key) key)) list)
          (setq list (cdr list)))
        (car list))))

(defun keats-update-save ()
  "First increases the number of updates. Then writes to file if
there has been enough changes. But only if `keats-save-at' is non nil."
  (when keats-save-at
    (setq keats-save-count (1+ keats-save-count))
    (if (>= keats-save-count keats-save-at)
        (keats-write))))

;;;###autoload
(define-minor-mode keats-mode
  "Simple interface to Emacs keybinding cheats."
  :init-value nil
  :keymap keats-mode-map
  (when keats-mode
    (unless (keats-file-exists-p)
      (switch-to-buffer (get-buffer-create keats-temp-buffer))
      (delete-region (point-min) (point-max))
      (write-file keats-file nil)
      (kill-this-buffer))
    (when (keats-file-valid-p)
      (keats-read))
    (add-hook 'kill-emacs-hook 'keats-write))
  (unless (keats-file-valid-p)
    (error "No valid keats file")))

(provide 'keats)

;;; keats.el ends here
