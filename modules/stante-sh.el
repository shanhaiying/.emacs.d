;;; stante-sh.el --- Stante Pede: Shell scripting support
;;; -*- coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (c) 2012 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stantepede.git
;; Version: 1.0.0
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
;; Set basic indentation offset to 2.
;;
;; Use C-c > to automatically update the indentation style to the conventions
;; used in the current buffer.

;;; Code:

(require 'stante-programming)

(eval-after-load 'sh-mode
  #'(setq-default 'sh-basic-offset 2))

;; Also consider .zsh files as `sh-mode' files.
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(provide 'stante-sh)

;;; stante-sh.el ends here
