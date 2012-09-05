;;; stante-emacs-lisp.el --- Stante Pede: Emacs LISP support
;; -*- coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (c) 2012 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stantepede.git
;; Version: 1.0.0
;; Keywords: convenience languages

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

;; Configure Emacs LISP editing.

;; Documentation
;; -------------
;;
;; Show information about a function or variable at point in the echo area (see
;; `eldoc-mode').

;; Balanced parenthesis
;; --------------------
;;
;; Keep parenthesis balanced while editing with `paredit-mode'.
;;
;; Read the reference table at http://mumble.net/~campbell/emacs/paredit.html
;; for a list of available commands.

;;; Code:

(require 'stante-autoloads)
(require 'stante-programming)

(package-install-if-needed 'paredit)

(eval-after-load 'lisp-mode
  '(progn

     (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
       (add-hook hook 'turn-on-eldoc-mode)
       (add-hook hook 'paredit-mode))

     (define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)
     ))

(provide 'stante-emacs-lisp)

;;; stante-emacs-lisp.el ends here
