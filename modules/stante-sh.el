;;; stante-sh.el --- Stante Pede Modules: Shell scripting support
;;; -*- coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (c) 2012 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stantepede.git
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

;; Provide support for shell scripting.

;; Indentation
;; -----------
;;
;; Define some standard shell script indentation styles:
;;
;; "zsh" defines the standard 2-space indentation used in most Zsh scripts.
;;
;; Use C-c s to load one of these styles for the current buffer.  Use C-c > to
;; automatically learn an indentation for the current buffer instead.

;; Keybindings
;; -----------
;;
;; C-c s in `sh-mode' loads an indentation style.

;;; Code:

(require 'stante-programming)

(after 'sh-script
  ;; Standard indentation styles
  (setq sh-styles-alist
        '(("zsh"
           (sh-basic-offset . 2)
           (sh-first-lines-indent . 0)
           (sh-indent-after-case . +)
           (sh-indent-after-do . +)
           (sh-indent-after-done . 0)
           (sh-indent-after-else . +)
           (sh-indent-after-if . +)
           (sh-indent-after-loop-construct . +)
           (sh-indent-after-open . +)
           (sh-indent-comment)
           (sh-indent-for-case-alt . ++)
           (sh-indent-for-case-label . +)
           (sh-indent-for-continuation . +)
           (sh-indent-for-do . 0)
           (sh-indent-for-done . 0)
           (sh-indent-for-else . 0)
           (sh-indent-for-fi . 0)
           (sh-indent-for-then . 0))))

  ;; Key binding to choose the style
  (define-key sh-mode-map (kbd "C-c s") 'sh-load-style))

;; Also consider .zsh files as `sh-mode' files.
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(provide 'stante-sh)

;;; stante-sh.el ends here
