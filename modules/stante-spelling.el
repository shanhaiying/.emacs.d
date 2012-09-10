;;; stante-spelling.el --- Stante Pede Modules: Spell checking
;; -*- coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (c) 2012 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stantepede.git
;; Version: 1.0.0
;; Keywords: abbrev convenience wp

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

;; Configure spell checking.

;; Spell checker
;; -------------
;;
;; Prefer the aspell tool from http://aspell.net/ as spell checker.
;;
;; This tool is not included in GNU Emacs, and needs to be installed
;; separately.
;;
;; On Linux install aspell and desired dictionaries from the package
;; repositories of your distribution.  For instance, on Ubuntu use "sudo
;; aptitude install aspell aspell-en aspell-de" to install aspell with English
;; and German dictionaries.
;;
;; On OS X install aspell with homebrew: "brew install aspell --lang=en,de".

;; Automatic spell checking
;; ------------------------
;;
;; Automatically spell-check text buffers.
;;
;; Automatically spell-check comments and strings in programming buffers.  This
;; only affects programming buffers whose major mode derives from `prog-mode'.
;; If you are writing a module for a programming mode that does not derive from
;; `prog-mode' you need to manually enable `flyspell-prog-mode' in the mode's
;; hook:
;;
;;     (eval-after-load 'stante-spelling
;;        #'(add-hook 'foo-mode-hook 'flyspell-prog-mode))
;;


;;; Code:

;; Just enable flyspell here.  ispell has reasonable defaults, so it doesn't
;; need any configuration.

(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(provide 'stante-spelling)

;;; stante-spelling.el ends here
