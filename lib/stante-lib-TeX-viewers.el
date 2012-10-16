;;; stante-lib-TeX-viewers.el --- Stante Pede Library: LaTeX viewer selection
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

;; LaTeX viewer selection.

;; `stante-TeX-select-view-programs' selects proper viewers for the current
;; platform.

;; Load `stante-lib-autoloads' to use the functions of this library.


;;; Code:

(require 'stante-lib-autoloads)
(require 'tex-buf)

(defun stante-find-skim-bundle ()
  "Return the location of the Skim bundle, or nil if Skim is not installed.

Skim is an advanced PDF viewer for OS X with SyncTex support.
See http://skim-app.sourceforge.net/ for more information."
  (stante-path-of-bundle "net.sourceforge.skim-app.skim"))

(defun stante-find-skim-displayline ()
  "Return the path of the displayline frontend of Skim.

Return nil if Skim is not installed.  See `stante-find-skim-bundle'."
  (let ((skim-bundle (stante-find-skim-bundle)))
    (when skim-bundle
      (concat (directory-file-name skim-bundle)
              "/Contents/SharedSupport/displayline"))))

(defun stante-TeX-find-view-programs-os-x ()
  "Find TeX view programs on OS X.

Populate `TeX-view-program-list' with installed viewers."
  ;; The default application, usually Preview
  (add-to-list 'TeX-view-program-list
               '("Default application" "open %o"))
  ;; Skim if installed
  (let ((skim-displayline (stante-find-skim-displayline)))
    (when skim-displayline
      (add-to-list 'TeX-view-program-list
                   `("Skim" (,skim-displayline " -b -r %n %o %b"))))))

(defun stante-TeX-select-view-programs-os-x ()
  "Select the best view programs on OS X.

Choose Skim if available, or fall back to the default application."
  ;; Find view programs
  (stante-TeX-find-view-programs-os-x)
  (setq TeX-view-program-selection
        `((output-dvi "Default application")
          (output-html "Default application")
          ;; Use Skim if installed for SyncTex support.
          (output-pdf ,(if (assoc "Skim" TeX-view-program-list)
                           "Skim" "Default application")))))

;;;###autoload
(defun stante-TeX-select-view-programs ()
  "Select the best view programs for the current platform."
  (when (stante-is-os-x)
    (stante-TeX-select-view-programs-os-x))
  )

(provide 'stante-lib-TeX-viewers)

;;; stante-lib-TeX-viewers.el ends here
