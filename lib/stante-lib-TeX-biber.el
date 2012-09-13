;;; stante-lib-TeX-biber.el --- Stante Pede Library: AUCTeX biber support
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

;; Biber support for AUCTeX.

;; Require this module to patch basic Biber support into AUCTeX.


;;; Code:

(defvar TeX-command-biber "Biber"
  "The name of the biber command.")

(defun TeX-run-Biber (name command file)
  "Create a process for NAME using COMMAND to format FILE with Biber."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function 'TeX-Biber-sentinel)
    (if TeX-process-asynchronous process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-Biber-sentinel (process name)
  "Cleanup TeX output buffer after running Biber.

PROCESS and NAME are ignored."
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
                 "^Package biblatex Warning: Please (re)run Biber" nil t))
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
(add-to-list 'LaTeX-clean-intermediate-suffixes "\\.run\\.xml")

(provide 'stante-lib-TeX-biber)

;;; stante-lib-TeX-biber.el ends here
