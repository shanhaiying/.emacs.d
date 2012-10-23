;;; stante-proofgeneral.el --- Stante Pede Modules: Proof General support
;;; -*- coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (c) 2012 Sebastian Wiesner
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

(require 'stante-lib-autoloads)

(defun stante-find-isabelle-binary-os-x (&optional binary)
  "Find the Isabelle process executable."
  (let ((directory (stante-path-of-bundle "de.tum.in.isabelle"))
        (binary (or binary "isabelle")))
    (when directory
      (executable-find
       (concat directory "/Contents/Resources/Isabelle/bin/" binary)))))

;; PG prompts for Isabelle executable when loading, hence we need to set the
;; command *before* loading PG.
(setq isa-isabelle-command (stante-find-isabelle-binary-os-x))

(after 'isabelle-system
  (when (stante-is-os-x)
    (setq
     isabelle-program-name-override (stante-find-isabelle-binary-os-x
                                     "isabelle-process"))))

(load (concat stante-dir "vendor/ProofGeneral/generic/proof-site.el"))

(after 'proof-useropts
  ;; Do not spam the frame with windows when executing a buffer
  (setq proof-three-window-enable nil))

(provide 'stante-proofgeneral)

;;; stante-proofgeneral.el ends here
