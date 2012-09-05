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

;;; Code:

(require 'package)
(require 'autoload)

;;;###autoload
(defun package-install-if-needed (name)
  "Install the package named NAME, unless it is already installed."
  (unless (package-installed-p name) (package-install name)))

;;;###autoload
;;;###autoload
(defun stante-update-autoload-file ()
  "Update the autoload file of Stante Pede."
  (interactive)
  (let ((generated-autoload-file stante-autoload-file))
    (update-directory-autoloads stante-modules-dir)))

(provide 'stante-helper)

;;; stante-helper.el ends here
