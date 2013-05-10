;;; stante-os-x.el --- Stante Pede: OS X support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012, 2013 Sebastian Wiesner
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
;; -----------------------
;;
;; `stante-id-of-bundle' gets the internal ID of an installed bundle.
;;
;; `stante-path-of-bundle' gets the installation path of an application bundle.
;;
;; `stante-homebrew-prefix' gets the prefix of Homebrew or an installed Homebrew
;; formula.
;;
;; `stante-homebrew-installed-p' determines whether Homebrew or a Homebrew
;; formula is installed.

;;; Code:

(defun stante-id-of-bundle (bundle)
  "Get the ID of a BUNDLE.

BUNDLE is the user-visible name of the bundle as string.  Return
the id of the bundle as string.

Do not use this function in code.  IDs are constant, hence use it
*during development* to determine the ID of the bundle, and then
hard-code the bundle ID in your code."
  (let ((script (format "id of app \"%s\"" bundle)))
    (car (process-lines "osascript" "-e" script))))

(defun stante-path-of-bundle (id)
  "Get the path of a bundle with ID.

ID is the bundle ID (see `stante-id-of-bundle' as string.  Return
the directory path of the bundle as string."
  (let ((query (format "kMDItemCFBundleIdentifier == '%s'" id)))
    (car (process-lines "mdfind" query))))

(defun stante-homebrew-prefix (&optional formula)
  "Get the homebrew prefix for FORMULA.

Without FORMULA, get the homebrew prefix itself.

Return nil, if homebrew is not available, or if the prefix
directory does not exist."
  (let ((prefix (condition-case nil
                    (car (apply #'process-lines "brew" "--prefix"
                                (when formula (list formula))))
                  (error nil))))
    (when (and prefix (file-directory-p prefix))
      prefix)))

(defun stante-homebrew-installed-p (&optional formula)
  "Determine whether a homebrew FORMULA is installed.

Without FORMULA determine whether Homebrew itself is available."
  (if formula
      (when (stante-homebrew-prefix formula) t)
    (when (executable-find "brew") t)))

(provide 'stante-os-x)

;; Local Variables:
;; coding: utf-8
;; End:

;;; stante-os-x.el ends here
