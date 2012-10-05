;;; stante-lib-os-x.el --- Stante Pede Library: OS X support
;;; -*- coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (c) 2012 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stante-pede.git
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

;; OS X support functions.

;; `stante-find-os-x-coreutils' searches for GNU Coreutils on OS X.

;; Load `stante-lib-autoloads' to use the functions of this library.

;;; Code:

;;;###autoload
(defun stante-find-os-x-coreutils ()
  "Return the directory containing the unprefixed GNU coreutils on OS X.

If the directory cannot be determined, return nil.

Currently this function only checks for coreutils installed with
homebrew.  In future, more sophisticated logic might be added."
  (condition-case nil
      (let ((prefix (car (process-lines "brew" "--prefix" "coreutils"))))
        (concat (directory-file-name prefix) "/libexec/gnubin"))
    (error nil)))

(provide 'stante-lib-os-x)

;;; stante-lib-os-x.el ends here
