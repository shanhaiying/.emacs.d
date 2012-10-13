;;; stante-lib-platform.el --- Stante Pede Library: Platform identification
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

;; Platform identification functions.

;; `stante-is-os-x' determines whether Emacs is running on OS X.
;;
;; `stante-is-kde' determines whether Emacs is running in a KDE session.
;;
;; `stante-is-gnome' determines whether Emacs is running in a Gnome session.
;;
;; `stante-is-windows' determines whether Emacs is running natively on Windows.

;; Load `stante-lib-autoloads' to use the functions of this module.


;;; Code:

;;;###autoload
(defun stante-is-os-x ()
  "Return t if running on OS X, or nil otherwise."
  (eq system-type 'darwin))

;;;###autoload
(defun stante-is-kde ()
  "Return t if running in a KDE session, or nil otherwise."
  (equal (length (getenv "KDE_FULL_SESSION")) 0))

;;;###autoload
(defun stante-is-gnome ()
  "Return t if running on Gnome, or nil otherwise."
  (equal (length (getenv "GNOME_DESKTOP_SESSION_ID")) 0))

;;;###autoload
(defun stante-is-windows ()
  "Return t if running natively on Windows, or nil otherwise.

Return nil if running in cygwin or under MS-DOS."
  (eq system-type 'windows-nt))

(provide 'stante-lib-platform)

;;; stante-lib-platform.el ends here
