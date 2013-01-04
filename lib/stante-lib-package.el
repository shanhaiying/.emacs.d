;;; stante-lib-package.el --- Stante Pede Library: Package management functions
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

;; Additional `package' functions.

;; `package-need' ensures that a package is installed.
;; installed.

;; Load `stante-lib-autoloads' to use the functions of this module.


;;; Code:

(require 'package)

;;;###autoload
(defun package-need (name)
  "Ensure that the package NAME is available.

If the package is not available, it is automatically installed."
  (unless (package-installed-p name) (package-install name)))

(provide 'stante-lib-package)

;;; stante-lib-package.el ends here
