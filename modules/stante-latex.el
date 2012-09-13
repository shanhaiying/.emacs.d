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
;; OS X Preview does not support SyncTex.  Install Skim from
;; http://skim-app.sourceforge.net/ and restart Emacs to use SyncTex on OS X.
;; Do not configure Skim as default PDF viewer (unless you want to do so).  This
;; module will automatically choose Skim if it is installed.

;;; Code:

(require 'stante-lib-autoloads)

(package-install-if-needed 'auctex)

(defun stante-find-skim-bundle ()
  "Return the location of the Skim bundle, or nil if Skim is not installed.

Skim is an advanced PDF viewer for OS X with SyncTex support.
See http://skim-app.sourceforge.net/ for more information."
  (car (process-lines "mdfind" "kMDItemCFBundleIdentifier \
== 'net.sourceforge.skim-app.skim'")))

(defun stante-find-skim-displayline ()
  "Return the path of the displayline frontend of Skim.

Return nil if Skim is not installed.  See `stante-find-skim-bundle'."
  (let ((skim-bundle (stante-find-skim-bundle)))
    (when skim-bundle
      (concat (directory-file-name skim-bundle)
              "/Contents/SharedSupport/displayline"))))

(defun stante-TeX-find-view-programs ()
  "Find TeX view programs on OS X.

Populate `TeX-view-program-list' with installed viewers."
  ;; The default application, usually Preview
  (add-to-list 'TeX-view-program-list
               '("Default application" "open %o"))
  ;; Skim if installed
  (let ((skim-displayline (stante-find-skim-displayline)))
    (when skim-displayline
      (add-to-list 'TeX-view-program-list
                   `("Skim" (,skim-displayline " -b -r %n %o %b"))))))

(defun stante-TeX-select-view-programs ()
  "Select the best view programs on OS X.

Choose Skim if available, or fall back to the default application."
  ;; Find view programs
  (stante-TeX-find-view-programs)
  (setq TeX-view-program-selection
        `((output-dvi "Default application")
          (output-html "Default application")
          ;; Use Skim if installed for SyncTex support.
          (output-pdf ,(if (assoc "Skim" TeX-view-program-list)
                           "Skim" "Default application")))))

;; OS X specific LaTeX setup, mostly viewer selection.  We prefer Skim if
;; installed, because it supports SyncTex.  Preview does not.
(when (stante-is-os-x)
  (eval-after-load 'tex #'(stante-TeX-select-view-programs)))


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

;; Configure RefTeX
(eval-after-load 'reftex
  #'(progn

      ;; Recommended optimizations
      (setq reftex-enable-partial-scans t ; Recommended optimizations
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
      ;; Use biblatex as default citation style
      (setq reftex-cite-format 'biblatex)
      ;; Make RefTeX recognize biblatex bibliographies
      (add-to-list 'reftex-bibliography-commands "addbibresource")))

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

;; HACK: Provide rough biblatex/biber support.  Should work for compiling, but
;; more advanced support is missing.  Look into using the patches provided by
;; the Biber author itself, and check how Auctex upstream works on this.
(eval-after-load 'tex-buf
  #'(progn

      (if (boundp 'TeX-command-biber)
          (message "Detected Biber support in AUCTeX.")

        (message "No Biber support in AUCTeX, enabling experimental support.")

        (defvar TeX-command-biber "Biber"
          "The name of the biber command.")

        (defun TeX-run-Biber (name command file)
          "Create a process for NAME using COMMAND to format FILE with Biber."
          (let ((process (TeX-run-command name command file)))
            (setq TeX-sentinel-function 'TeX-Biber-sentinel)
            (if TeX-process-asynchronous
                process
              (TeX-synchronous-sentinel name file process))))

        (defun TeX-Biber-sentinel (process name)
          "Cleanup TeX output buffer after running Biber."
          (goto-char (point-max))
          (cond
           ;; Check whether Biber reports any warnings or errors.
           ((re-search-backward (concat
                                 "^(There \\(?:was\\|were\\) \\([0-9]+\\) "
                                 "\\(warnings?\\|error messages?\\))") nil t)
            ;; Tell the user their number so that she sees whether the
            ;; situation is getting better or worse.
            (message (concat "Biber finished with %s %s. "
                             "Type `%s' to display output.")
                     (match-string 1) (match-string 2)
                     (substitute-command-keys
                      "\\\\[TeX-recenter-output-buffer]")))
           (t
            (message (concat "Biber finished successfully. "
                             "Run LaTeX again to get citations right."))))
          (setq TeX-command-next TeX-command-default))

        (defadvice TeX-LaTeX-sentinel (around TeX-LaTeX-sentinal-need-biber)
          "Run Biber if necessary."
          (cond ((and (save-excursion
                        (re-search-forward
                         "^Package biblatex Warning: Please (re)run Biber"
                         nil t))
                      (with-current-buffer TeX-command-buffer
                        (TeX-check-files (TeX-master-file "bcf")
                                         (TeX-style-list)
                                         (append TeX-file-extensions
                                                 BibTeX-file-extensions))))
                 (message "%s%s" "You should run Biber to get citations right, "
                          (TeX-current-pages))
                 (setq TeX-command-next (with-current-buffer TeX-command-buffer
                                          TeX-command-biber)))
                ((save-excursion
                   (re-search-forward
                    "^Package biblatex Warning: Please rerun LaTeX\\." nil t))
                 (message "%s%s" "You should run LaTeX to get citations right, "
                          (TeX-current-pages))
                 (setq TeX-command-next (with-current-buffer TeX-command-buffer
                                          TeX-command-default)))
                (t ad-do-it)))
        (ad-activate 'TeX-LaTeX-sentinel t)

        ;; Declare Biber command
        (unless (assoc TeX-command-biber TeX-command-list)
          (add-to-list 'TeX-command-list
                       `(,TeX-command-biber "biber %s" TeX-run-Biber nil t
                                            :help "Run Biber")))

        ;; Clean Biber files
        (add-to-list 'LaTeX-clean-intermediate-suffixes "\\.bcf")
        (add-to-list 'LaTeX-clean-intermediate-suffixes "\\.run\\.xml"))))

(provide 'stante-latex)

;;; stante-latex.el ends here
