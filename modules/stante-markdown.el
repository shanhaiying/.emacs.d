;;; stante-markdown.el --- Stante Pede Modules: Markdown support
;;; -*- coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (c) 2012 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stantepede.git
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

;; Provide support for editing and previewing markdown.
;;
;; Markdown editing is provided by `markdown-mode' from
;; https://github.com/milkypostman/markdown-mode.

;; Markdown processor
;; ------------------
;;
;; Search for the following markdown processors in `exec-path':
;;
;; - kramdown (http://kramdown.rubyforge.org/)
;; - markdown2 (https://github.com/trentm/python-markdown2)
;; - pandoc (http://johnmacfarlane.net/pandoc/index.html)
;;
;; If none of the above is found, emit a warning message and fall back to the
;; default "markdown" utility.


;;; Code:

(require 'stante-lib-autoloads)
(require 'stante-text)
(require 'cl)

(package-need 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(defun stante-find-markdown-processor ()
  "Find a suitable markdown processor."
  (let ((processor (some 'executable-find
                         '("kramdown"
                           "markdown2"
                           "pandoc"))))
    (if processor
        (setq markdown-command "kramdown")
      (message "No markdown processor found, falling back to default %s"
               markdown-command))))

(after 'markdown-mode
  (stante-find-markdown-processor))

(provide 'stante-markdown)

;;; stante-markdown.el ends here
