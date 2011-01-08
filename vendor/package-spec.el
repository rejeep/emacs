;;; package-spec.el --- Specify package.el packages to use in a package spec

;; Copyright (C) 2010 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: speed, convenience
;; URL: http://github.com/rejeep/package-spec

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

;; With package-spec.el you can specify what package.el sources and
;; packages you want. When starting Emacs all packages specified in
;; the gemfile will be installed if not already, and loaded.
;;
;; To install:
;;   (add-to-list 'load-path "/path/to/package-spec")
;;   (require 'package-spec)

;;; Code:

(require 'package)

(defvar package-spec-file-name "package-spec.el"
  "Name of package spec file.")

(defvar package-spec-packages ()
  "List of packages defined in package spec.")

(defun package-spec-install ()
  "Install packages specified in `package-spec-file-name'."
  (interactive)
  (dolist (package package-spec-packages)
    (unless (package-installed-p package)
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(defun package-spec-load ()
  "Load `package-spec-file-name'."
  (let ((package-spec (expand-file-name package-spec-file-name "~/.emacs.d")))
    (if (file-exists-p package-spec)
        (load package-spec)
      (error "Missing package spec file."))))

(defun source (name url)
  "Add package source with NAME and URL."
  (add-to-list 'package-archives `(,name . ,url) t))

(defun package (name)
  "Add package with NAME."
  (add-to-list 'package-spec-packages (intern name)))

(package-initialize)
(package-spec-load)
(package-refresh-contents)
(package-spec-install)

(provide 'package-spec)

;;; package-spec.el ends here
