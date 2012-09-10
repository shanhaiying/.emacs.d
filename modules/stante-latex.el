;;; stante-latex.el --- Stante Pede Modules: LaTeX support
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

;; Provide LaTeX editing support.

;; LaTeX editing is based by the powerful Auctex package.  See
;; http://www.gnu.org/software/auctex/ for more information, including
;; comprehensive documentation.

;; Engine
;; ------
;;
;; Set XeTeX engine in PDF mode as default for full Unicode support.

;; Synctex
;; -------
;;
;; Enable forward and inverse search via SyncTex.


;;; Code:

(require 'stante-autoloads)

(package-install-if-needed 'auctex)

(eval-after-load 'tex-site
  #'(progn
      (setq TeX-auto-save t ; Autosave documents
            ;; Parse document structure
            TeX-parse-self t
            ;; Use SyncTeX for source correlation
            TeX-source-correlate-method 'synctex
            ;; Enable source correlation mode
            TeX-source-correlate-mode t)
      (setq-default TeX-master nil ; Ask for master document
                    ;; Use a modern LaTeX engine to build PDFs
                    TeX-engine 'xetex
                    TeX-PDF-mode t)
      ))

(provide 'stante-latex)

;;; stante-latex.el ends here
