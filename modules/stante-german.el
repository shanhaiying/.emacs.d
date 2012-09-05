;;; stante-german.el --- Stante Pede: Support for German language environments
;;
;; Copyright (c) 2012 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stantepede.git
;; Version: 1.0.0
;; Keywords: convenience

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

;; Improve experience on German keyboard layouts.

;; Remappings
;; ----------
;;
;; Map some commands to better reachable keys.
;;
;; C-M-ö indents the current region (see `indent-region').
;;
;; C-M-ä expands the word under point with `hippie-expand'.


;;; Code:

(global-set-key (kbd "C-M-ö") 'indent-region)
(global-set-key (kbd "C-M-ä") 'hippie-expand)

(provide 'stante-german)

;;; stante-editor.el ends here
