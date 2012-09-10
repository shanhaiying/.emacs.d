;;; stante-snippets.el --- Stante Pede Modules: Text snippets
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

;; Configure snippets.

;; Install `yasnippet' to expand little text snippets.  See
;; https://github.com/capitaomorte/yasnippet for more information about
;; yasnippet.

;; Snippets
;; --------
;;
;; Load builtin snippets of yasnippet and user-created snippets from the
;; "snippets/" directory of Stante Pede (see `stante-snippets-dir').
;;
;; Create custom snippets in `stante-snippets-dir'.

;; Keybindings
;; -----------
;;
;; Tab expands snippets if the word under point is a snippet trigger.  Otherwise
;; Tab does what the current mode wants it to do.


;;; Code:

(require 'stante-autoloads)

(defconst stante-snippets-dir (concat stante-dir "snippets/")
  "The directory where Stante Pede snippets are stored.")

(package-install-if-needed 'yasnippet)
(require 'yasnippet)
(eval-after-load 'yasnippet
  #'(progn
      ;; Create the Stante Pede snippet directory if necessary.  This is to be
      ;; removed as soon as we're shipping custom snippets in this directory.
      (unless (file-directory-p stante-snippets-dir)
        (make-directory stante-snippets-dir))
      (setq yas-snippet-dirs
            `(,stante-snippets-dir ; Our snippets first
              ,(concat (file-name-directory yas--load-file-name) "snippets")))
      ;; Enable yasnippet in all modes.
      (yas-global-mode 1)))

(provide 'stante-snippets)

;;; stante-snippets.el ends here
