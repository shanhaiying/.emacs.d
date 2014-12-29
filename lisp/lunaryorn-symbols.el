;;; lunaryorn-symbols.el --- Utilities for symbols  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://gihub.com/lunaryorn/.emacs.d

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

;; Utilities for working with symbols

;;; Code:

(require 'highlight-symbol)

(defun highlight-symbol-ag ()
  "Call `ag-project-regexp' with the symbol at point.

Needs ag.el from URL `https://github.com/Wilfred/ag.el'."
  (interactive)
  (unless (fboundp 'ag-project)
    (error "Please install ag.el from https://github.com/Wilfred/ag.el"))
  (if (thing-at-point 'symbol)
      (let ((highlight-symbol-border-pattern '("\\b" . "\\b")))
        (ag-project-regexp (highlight-symbol-get-symbol)))
    (error "No symbol at point")))

(provide 'lunaryorn-symbols)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; lunaryorn-symbols.el ends here
