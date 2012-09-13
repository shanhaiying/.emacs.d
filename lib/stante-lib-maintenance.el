;;; stante-lib-maintenance.el --- Stante Pede Library: Maintenance functions
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

;; Stante Pede maintenance.

;; `stante-update-autoload-file' updates the autoload definitions of Stante
;; Pede.
;;
;; `stante-byte-recompile' byte-compiles all Stante Pede modules.

;; Load `stante-lib-autoloads' to use the functions of this library.

;;; Code:

(require 'autoload)

;;;###autoload
(defconst stante-autoload-file (concat stante-lib-dir "stante-lib-autoloads.el")
  "Location of the autoload file for the Stante Pede Library.")

;;;###autoload
(defun stante-update-autoload-file ()
  "Update the autoload file of Stante Pede."
  (interactive)
  (let ((generated-autoload-file stante-autoload-file))
    (update-directory-autoloads stante-lib-dir)))

;;;###autoload
(defun stante-byte-recompile ()
  "Byte-compile all modules of Stante pede."
  (interactive)
  (byte-recompile-directory stante-lib-dir 0)
  (byte-recompile-directory stante-modules-dir 0)
  (byte-recompile-file stante-init-file nil 0))

(provide 'stante-lib-maintenance)

;;; stante-lib-maintenance.el ends here
