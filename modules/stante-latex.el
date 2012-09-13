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
;;
;; Support Biber as BibTeX backend for full Unicode support.

;; References and citations
;; ------------------------
;;
;; Enable RefTeX to manage citations.  See the RefTeX manual at
;; http://www.gnu.org/software/auctex/manual/reftex/index.html for more
;; information.
;;
;; Configure RefTeX to support the biblatex package.
;;
;; Optimize RefTeX configuration for large documents.  Save parse state when
;; killing LaTeX buffers.  This will create "MASTER.rel" along with your
;; "MASTER.tex" file.

;; Synctex
;; -------
;;
;; Enable forward and inverse search via SyncTex.
;;
;; The PDF reader will open the PDF at the line under cursor, and provide means
;; to jump to the corresponding source file line in Emacs.  You need a PDF
;; viewer that supports SyncTex to use this feature:
;;
;; On OS X install Skim from http://skim-app.sourceforge.net/ and restart Emacs.
;; The default OS X Preview application does not support SyncTex.  You do not
;; neet to configure Skim as default PDF viewer, this module will automatically
;; find Skim if it is installed.


;;; Code:

(require 'stante-lib-autoloads)

;; Install and configure *the* LaTeX environment
(package-install-if-needed 'auctex)

(eval-after-load 'tex-site
  #'(progn
      (setq TeX-auto-save t             ; Autosave documents
            ;; Parse document structure
            TeX-parse-self t
            ;; Use SyncTeX for source correlation
            TeX-source-correlate-method 'synctex
            ;; Enable source correlation mode
            TeX-source-correlate-mode t)
      (setq-default TeX-master nil    ; Ask for master document
                    ;; Use a modern LaTeX engine to build PDFs
                    TeX-engine 'xetex
                    TeX-PDF-mode t)

      ;; Enable on-the-fly checking for latex documents
      (add-hook 'LaTeX-mode-hook 'flymake-mode-on)
      (add-hook 'LaTeX-mode-hook 'turn-on-reftex)))

;; Select best viewing programs
(eval-after-load 'tex #'(stante-TeX-select-view-programs))


;; Configure RefTeX
(eval-after-load 'reftex
  #'(progn
      ;; Recommended optimizations
      (setq reftex-enable-partial-scans t
            reftex-save-parse-info t
            reftex-use-multiple-selection-buffers t
            ;; Plug RefTeX into AUCTeX
            reftex-plug-into-AUCTeX t)

      (unless (assq 'biblatex reftex-cite-format-builtin)
        ;; Add biblatex support if not already builtin
        (add-to-list 'reftex-cite-format-builtin
                     '(biblatex "The biblatex package"
                                ((?\C-m . "\\cite[]{%l}")
                                 (?t . "\\textcite{%l}")
                                 (?a . "\\autocite[]{%l}")
                                 (?p . "\\parencite{%l}")
                                 (?f . "\\footcite[][]{%l}")
                                 (?F . "\\fullcite[]{%l}")
                                 (?x . "[]{%l}")
                                 (?X . "{%l}")))))
      ;; Use biblatex as default citation style
      (setq reftex-cite-format 'biblatex)
      ;; Make RefTeX recognize biblatex bibliographies
      (add-to-list 'reftex-bibliography-commands "addbibresource")))

;; Plug RefTeX into bib-cite
(eval-after-load 'bib-cite #'(setq bib-cite-use-reftex-view-crossref t))


;; HACK: Provide rough biblatex/biber support.  Should work for compiling, but
;; more advanced support is missing.  Look into using the patches provided by
;; the Biber author itself, and check how Auctex upstream works on this.
(eval-after-load 'tex-buf
  #'(if (boundp 'TeX-command-biber)
        (message "Detected Biber support in AUCTeX.")

      (message "No Biber support in AUCTeX, enabling experimental support.")
      (require 'stante-lib-TeX-biber)))


(defun flymake-get-tex-args-chktex (filename)
  "Get the command to check TeX documents on the fly."
  `("chktex" ("-v0" "-q" "-I",filename)))

(eval-after-load 'flymake
  #'(progn
      ;; Override the default flymake syntax checking for LaTeX to use chktex
      (fset 'flymake-get-tex-args 'flymake-get-tex-args-chktex)

      ;; Treat master/child documents like simple documents because chktex
      ;; doesn't do a full compilation
      (fset 'flymake-master-tex-init 'flymake-simple-tex-init)))


(provide 'stante-latex)

;;; stante-latex.el ends here
