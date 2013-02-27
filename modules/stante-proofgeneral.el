;;; stante-proofgeneral.el --- Stante Pede Modules: Proof General support -*- lexical-binding: t; -*-
;;; -*- coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (c) 2012, 2013 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stante-pede.git
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

;; Provide support for Proof General
;;
;;; Code:

(defconst stante-proofgeneral-dir
  (expand-file-name "vendor/ProofGeneral" stante-dir)
  "The directory of Proof General.")

(defun stante-find-isabelle-binary-os-x (&optional binary)
  "Find the Isabelle process executable."
  (let ((bundle-directory (stante-path-of-bundle "de.tum.in.isabelle"))
        (binary (or binary "isabelle")))
    (when bundle-directory
      (let ((binary-directory
             (expand-file-name "Contents/Resources/Isabelle/bin"
                               bundle-directory)))
        (executable-find (expand-file-name binary binary-directory))))))

;; PG prompts for Isabelle executable when loading, hence we need to set the
;; command *before* loading PG.
(when (stante-is-os-x)
  (setq isa-isabelle-command (stante-find-isabelle-binary-os-x)))

(after 'isabelle-system
  (when (stante-is-os-x)
    (setq
     isabelle-program-name-override (stante-find-isabelle-binary-os-x
                                     "isabelle-process"))))

(load (expand-file-name "generic/proof-site.el" stante-proofgeneral-dir))

(after 'proof-useropts
  ;; Do not spam the frame with windows when executing a buffer
  (setq proof-three-window-enable nil))

(after 'isar-syntax
  (set-face-attribute 'isabelle-string-face nil
                      :foreground nil :background nil
                      :inherit 'font-lock-string-face)
  (set-face-attribute 'isabelle-quote-face nil
                      :foreground nil :background nil
                      :inherit 'font-lock-string-face))

(provide 'stante-proofgeneral)

;; Local Variables:
;; coding: utf-8
;; End:

;;; stante-proofgeneral.el ends here
