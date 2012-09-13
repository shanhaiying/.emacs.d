;;; stante-completion.el --- Stante Pede Modules: Auto completion
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

;; Configure auto completion.

;; Auto completion is provided by `auto-complete-mode'.  See
;; https://github.com/auto-complete/auto-completefor more information, and read
;; the manual at http://cx4a.org/software/auto-complete/manual.html.

;; Completion
;; -----------
;;
;; Complete the word under point reasonably.  The exact way of auto completion
;; depend on the major mode.

;; Keybindings
;; -----------
;;
;; Tab triggers completion, or falls back to its normal operation if the word
;; under point cannot complete.


;;; Code:

(require 'stante-lib-autoloads)

(package-install-if-needed 'auto-complete)
(require 'auto-complete-config)
(eval-after-load 'auto-complete-config
  #'(progn
      ;; Configure default sources
      (setq-default ac-sources '(ac-source-yasnippet
                                 ac-source-abbrev
                                 ac-source-words-in-same-mode-buffers
                                 ac-source-dictionary)
                    ;; Set a default trigger key
                    ac-trigger-key "TAB"
                    ;; Move history file to property directory
                    ac-comphist-file (concat stante-var-dir "ac-comphist"))
      (global-auto-complete-mode 1)
      ))

(provide 'stante-completion)

;;; stante-completion.el ends here
