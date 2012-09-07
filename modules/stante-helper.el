;;; stante-helper.el --- Stante Pede: Utility functions for all modules
;;; -*- coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (c) 2012 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stantepede.git
;; Version: 1.0.0
;; Keywords: extensions

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.


;;; Commentary:

;; Provide utility functions.  Use `stante-autoloads' instead of loading this
;; module, to only load this file as needed.

;; Package management
;; ------------------
;;
;; `package-install-if-needed' installs a package if it is not already
;; installed.

;; Strings
;; -------
;;
;; `stante-string-trim' removes leading and trailing whitespace from a string.

;; IO utilities
;; ------------
;;
;; `stante-get-file-contents' gets the contents of a file as string.
;;
;; `stante-set-file-contents' sets the contents of a file.
;;
;; `stante-get-file-lines' gets the lines in a file as list.

;; Maintenaince
;; ------------
;;
;; `stante-update-autoload-file' updates the autoload definitions of Stante
;; Pede.
;;
;; `stante-byte-recompile' byte-compiles all Stante Pede modules.

;;; Code:

(require 'package)
(require 'autoload)

;;;###autoload
(defun package-install-if-needed (name)
  "Install the package named NAME, unless it is already installed."
  (unless (package-installed-p name) (package-install name)))

;;;###autoload
(defun stante-string-trim (str)
  "Remove leading and trailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

;;;###autoload
(defun stante-get-file-contents (filename)
  "Return the contents of the file FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

;;;###autoload
(defun stante-set-file-contents (filename contents)
  "Set the contents of the file FILENAME.

Create the file FILENAME if it does not exist, or completely
overwrite it if it does."
  (with-temp-buffer
    (insert contents)
    (write-region (point-min) (point-max) filename nil 0)))

;;;###autoload
(defun stante-get-file-lines (filename)
  "Return a list of lines of file FILENAME."
  (split-string (stante-get-file-contents filename) "\n" t))

;;;###autoload
(defun stante-update-autoload-file ()
  "Update the autoload file of Stante Pede."
  (interactive)
  (let ((generated-autoload-file stante-autoload-file))
    (update-directory-autoloads stante-modules-dir)))

;;;###autoload
(defun stante-byte-recompile ()
  "Byte-compile all modules of Stante pede."
  (interactive)
  (byte-recompile-directory stante-modules-dir 0)
  (byte-recompile-file stante-init-file nil 0))

(provide 'stante-helper)

;;; stante-helper.el ends here
