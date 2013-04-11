;;; init.el --- Stante Pede: Instantly awesome Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012, 2013 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stante-pede.git
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

;; Your personal Emacs configuration.  Load Stante Pede, and choose your modules
;; wisely.


;;; Code:

;; Load Stante Pede
(load (locate-user-emacs-file "stante-init"))

;; Load Stante modules as you like
;; Basic modules
(require 'stante-ui)
(require 'stante-os-x)
(require 'stante-editor)
(require 'stante-spelling)
(require 'stante-completion)
(require 'stante-snippets)

;; Tools and utilities
(require 'stante-tools)
(require 'stante-utilities)
(require 'stante-git)

;; Programming languages
(require 'stante-emacs-lisp)
(require 'stante-python)
(require 'stante-coffee)
(require 'stante-sh)
(require 'stante-haskell)

;; Markup languages
(require 'stante-tex)
(require 'stante-markdown)
(require 'stante-org)
(require 'stante-xml)
(require 'stante-sass)

;; Sciene
(require 'stante-proofgeneral)

;; Localization support
(require 'stante-german)

;; Choose a nice color theme.
;;
;; WARNING:  Loading themes can execute *arbitrary* code.  Do NEVER load themes
;; from untrusted sources!

;; An eye-friendly light theme
(load-theme 'solarized-light t)
;;(load-theme 'solarized-dark t)
;;(load-theme 'zenburn t)

;; Customize as you like here.  Please try to put your configuration into Stante
;; modules, or even write new Stante modules, and contribute these changes back
;; via Github Pull Requests for the profit of other users.

;; Local Variables:
;; coding: utf-8
;; End:

;;; init.el ends here
