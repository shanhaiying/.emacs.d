;;; stante-text.el --- Stante Pede Modules: Basic plain text editing
;;
;; Copyright (c) 2012 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stante-pede.git
;; Keywords: convenience languages tools wp

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

;; Setup text editing for modes based on `text-mode'.

;; This module is automatically loaded by Stante Pede modules that configure
;; specific text editing modes so you should normally not need to enable it
;; explicitly.  However, if you are using a text editing mode that has no
;; specific Stante Pede module, you might want to load this module to enable
;; some basic tweaks.  In this case, please consider writing a proper Stante
;; Pede module for this mode and contribute it to the Stante Pede project.

;; Filling
;; -------
;;
;; Automatically fill text with `auto-fill-mode'.

;; Whitespace
;; ----------
;;
;; Highlight whitespace in text buffers (see `whitespace-mode') and remove
;; trailing whitespace after saving.


;;; Code:

(defun stante-text-whitespace ()
  "Enable whitespace mode for current buffer.

Also arrange for trailing whitespace to be removed before saving."
  (whitespace-mode 1)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(after "text-mode"
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'text-mode-hook 'stante-text-whitespace))

(provide 'stante-text)

;;; stante-text.el ends here
