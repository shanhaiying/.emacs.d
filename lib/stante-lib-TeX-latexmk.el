;;; stante-lib-TeX-latexmk.el --- Stante Pede Library: AUCTeX latexmk support
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

;; Allow compilation with "latexmk" from AUCTeX


;;; Code:

(require 'tex)
(require 'latex)

(defvar TeX-command-latexmk "latexmk"
  "The name of the latexmk command.")

;; Declare Biber command
(unless (assoc TeX-command-latexmk TeX-command-list)
  (add-to-list 'TeX-command-list
               `(,TeX-command-latexmk "latexmk" TeX-run-command t t
                                    :help "Run latexmk")))

;; Clean Biber files
(dolist (ext '("\\.fdb_latexmk" "\\.fls"))
  (add-to-list 'LaTeX-clean-intermediate-suffixes ext))

(provide 'stante-lib-TeX-latexmk)

;;; stante-lib-TeX-latexmk.el ends here
