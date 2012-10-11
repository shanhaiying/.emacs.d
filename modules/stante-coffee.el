;;; stante-coffee.el --- Stante Pede Modules: CoffeeScript support
;;; -*- coding: utf-8; lexical-binding: t -*-
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

;; Provide support for CoffeeScript.
;;
;; CoffeeScript editing is provided by coffee-mode from
;; https://github.com/defunkt/coffee-mode

;;; Code:

(require 'stante-lib-autoloads)
(require 'stante-programming)

(package-need 'coffee-mode)

(after 'coffee-mode
  ;; CoffeeScript should use two spaces for indentation
  (setq coffee-tab-width 2)

  ;; FIXME: CoffeeScript does not derive from prog-mode, see
  ;; https://github.com/defunkt/coffee-mode/issues/93
  (stante-setup-programming-mode-hook 'coffee-mode-hook)

  (add-hook 'coffee-mode-hook 'flymake-mode-on))

(provide 'stante-coffee)

;;; stante-coffee.el ends here
