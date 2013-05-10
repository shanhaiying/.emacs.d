;;; stante-markdown.el --- Stante Peed: Markdown functions -*- lexical-binding: t; -*-
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

;; Support modes for text-editing modes.

;;; Code:

(require 'dash)
(require 'markdown-mode)

(defconst stante-markdown-commands
  '(("kramdown")
    ("markdown2" "-x" "fenced-code-blocks")
    ("pandoc"))
  "Markdown processors we try to use.")

;;;###autoload
(defun stante-find-markdown-processor ()
  "Find a suitable markdown processor.

Search for a suitable markdown processor using
`stante-markdown-commands' and set `markdown-command' properly.

Return the new `markdown-command' or signal an error if no
suitable processor was found."
  (interactive)
  ;; Clear previous command
  (setq markdown-command
        (mapconcat #'shell-quote-argument
                   (--first (executable-find (car it)) stante-markdown-commands)
                   " "))
  (unless markdown-command
    (error "No markdown processor found"))
  markdown-command)

(provide 'stante-markdown)

;; Local Variables:
;; coding: utf-8
;; End:

;;; stante-markdown.el ends here
