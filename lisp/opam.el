;;; opam.el --- OPAM tools for Emacs                 -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; OPAM tools for Emacs.
;;
;; See URL `http://opam.ocamlpro.com/'.

;;; Code:

(require 'subr-x)

(defun opam-init ()
  "Initialize OPAM in this Emacs.

See URL `http://opam.ocamlpro.com/' for more information about
OPAM."
  (with-temp-buffer
    (when-let (opam (executable-find "opam"))
      (let ((exit-code (call-process "opam" nil t nil "config" "env" "--sexp")))
        (if (not (equal exit-code 0))
            (lwarn 'opam :warning "opam config env failed with exit code %S and output:
%s" exit-code (buffer-substring-no-properties (point-min) (point-max)))
          (goto-char (point-min))
          (let ((sexps (read (current-buffer))))
            (skip-chars-forward "[:space:]")
            (unless (eobp)
              (lwarn 'opam :warning "Trailing text in opam config env:\n%S"
                     (buffer-substring-no-properties (point) (point-max))))
            (pcase-dolist (`(,var ,value) sexps)
              (setenv var value)))))))
  ;; Now update `exec-path' and `load-path'
  (setq exec-path (append (parse-colon-path (getenv "PATH"))
                          (list exec-directory))))

(provide 'opam)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; opam.el ends here
