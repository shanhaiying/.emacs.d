;;; stante-latex.el --- Stante Pede Modules: LaTeX support
;;
;; Copyright (c) 2012, 2013 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stante-pede.git
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
;;
;; Install the BibTeX manager EBib from http://ebib.sourceforge.net/.  Use M-x
;; ebib to start EBib and read the manual at
;; http://ebib.sourceforge.net/manual/ebib-manual.html.

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

;; latexmk
;; -------
;;
;; Provide support for compilation with the popular "latexmk" utility via C-c
;; C-c latexmk.


;;; Code:

(require 'stante-text)
(require 'dash)

;; Install and configure *the* LaTeX environment
(package-need 'auctex)

;; Handle .latex files with AUCTeX, too.
(add-to-list 'auto-mode-alist '("\\.[lL]a[tT]e[xX]\\'" . latex-mode))

(after 'tex-site
  (setq TeX-auto-save t        ; Autosave documents
        TeX-parse-self t       ; Parse documents
        TeX-save-query nil     ; Don't ask before saving
        TeX-clean-confirm nil  ; Don't ask before cleaning
        ;; Enable forward and inverse search with SyncTeX
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex)
  (setq-default TeX-master nil ; Ask for master document
                ;; Use a modern LaTeX engine to build PDFs
                TeX-engine 'xetex
                TeX-PDF-mode t)

  ;; Setup sub modes
  (--each '(turn-on-reftex LaTeX-math-mode) (add-hook 'LaTeX-mode-hook it)))

;; Select best viewing programs
(after 'tex (stante-TeX-select-view-programs))

;; Configure RefTeX
(after 'reftex
  (setq reftex-plug-into-AUCTeX t
        ;; Recommended optimizations
        reftex-enable-partial-scans t
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t)

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
  (setq reftex-cite-format 'biblatex) ; Use Biblatex as default citation style
  ;; Make RefTeX recognize biblatex bibliographies
  (add-to-list 'reftex-bibliography-commands "addbibresource"))

(after 'bib-cite
  (setq bib-cite-use-reftex-view-crossref t))

;; Configure bibtex editing to use biblatex by default
(after 'bibtex
  (bibtex-set-dialect 'biblatex)
  ;; Configure exhaustive cleanup of bibtex entries
  (setq bibtex-entry-format '(opts-or-alts
                              required-fields
                              numerical-fields
                              whitespace
                              realign
                              last-comma
                              delimiters
                              unify-case
                              strings
                              sort-fields)))

;; BibTeX manager
(package-need 'ebib)

(after 'ebib
  (setq ebib-autogenerate-keys t))


;; Provide latexmk support.
(after 'tex
  (unless (boundp 'TeX-command-latexmk) ; Just in case this ever gets upstreamed
    (defvar TeX-command-latexmk "latexmk"
      "The name of the latexmk command.")

    ;; Declare the latexmk command
    (unless (assoc TeX-command-latexmk TeX-command-list)
      (add-to-list 'TeX-command-list
                   `(,TeX-command-latexmk "latexmk" TeX-run-command t t
                                          :Help "Run latexmk"))))

  ;; Replace lacheck with chktex for "Check" command
  (setcar (cdr (assoc "Check" TeX-command-list))  "chktex -v3 -q -I %s"))

(after 'latex
  ;; Clean latexmk files
  (--each '("\\.fdb_latexmk" "\\.fls")
    (add-to-list 'LaTeX-clean-intermediate-suffixes it)))

(provide 'stante-latex)

;;; stante-latex.el ends here
