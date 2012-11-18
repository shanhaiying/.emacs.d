;;; stante-haskell.el --- Stante Pede Modules: Haskell support
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

;; Provide support for Haskell.
;;
;; Haskell editing is provided by haskell-mode from
;; https://github.com/haskell/haskell-mode.

;;; Code:

(require 'stante-lib-autoloads)
(require 'stante-programming)

(package-need 'haskell-mode)

(after 'haskell-mode
  ;; FIXME: Haskell mode does not derive from prog-mode yet, see
  ;; https://github.com/haskell/haskell-mode/pull/86
  (stante-setup-programming-mode-hook 'haskell-mode-hook)
  (after 'stante-spelling
    (add-hook 'haskell-mode-hook 'flyspell-prog-mode))

  (dolist (hook '(turn-on-haskell-indentation
                  turn-on-haskell-doc-mode
                  turn-on-haskell-decl-scan))
    (add-hook 'haskell-mode-hook hook)))

(provide 'stante-haskell)

;;; stante-haskell.el ends here
