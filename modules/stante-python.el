;;; stante-python.el --- Stante Pede Modules: Python support -*- lexical-binding: t; -*-
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

;; Provide support for Python.
;;
;; Python editing is provided by the python-mode from
;; https://github.com/fgallina/python.el

;;; Code:

(require 'stante-programming)

(defconst stante-python-checkers '("flake8" "epylint" "pyflakes")
  "Python checking tools.

Candidates for `python-check-command'.")

(defun stante-python-filling ()
  "Configure filling for Python."
  ;; PEP 8 recommends a maximum of 79 characters
  (setq fill-column 79))

;; Find the best checker
(after 'python
  (add-hook 'python-mode-hook 'stante-python-filling)
  (add-hook 'python-mode-hook 'subword-mode)

  (setq python-check-command (-first 'executable-find stante-python-checkers))

  ;; Default to Python 3 if available
  (let ((python3 (executable-find "python3")))
    (when python3
      (setq python-shell-interpreter python3))))

(after 'expand-region
  ;; Tell expand-region about the Python mode we're using
  (setq expand-region-guess-python-mode nil
        expand-region-preferred-python-mode 'fgallina-python))

(provide 'stante-python)

;; Local Variables:
;; coding: utf-8
;; End:

;;; stante-python.el ends here
