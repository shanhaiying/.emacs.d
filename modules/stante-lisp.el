;;; stante-lisp.el --- Stante Pede Modules: General LISP support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2013 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stante-pede.git
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

;; Configure general LISP editing.

;; This module is automatically loaded by Stante Pede modules that configure
;; LISP-like programming languages.  You do not need to enable it explicitly.

;; Keybindings
;; -----------
;;
;; M-( wraps a sexp in parenthesis.

;; Reusability
;; -----------
;;
;; Use `stante-setup-lispy-mode-hook' to setup a hook of a LISP-like programming
;; language.

;;; Code:

(require 'stante-programming)

(require 'dash)

(defun stante-setup-lispy-mode-hook (hook)
  "Setup HOOK for lispy programming.

Enable Rainbow delimiters."
  (--each '(rainbow-delimiters-mode)
    (add-hook hook it)))

(after 'smartparens
  (sp-with-modes sp--lisp-modes
    (sp-local-pair "(" nil :bind "M-(")))

;; De-clutter mode line
(after 'rainbow-delimiters (diminish 'rainbow-delimiters-mode))

(provide 'stante-lisp)

;; Local Variables:
;; coding: utf-8
;; End:

;;; stante-lisp.el ends here
