;;; lunaryorn-markdown.el --- Additional tools for Markdown  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://gihub.com/lunaryorn/.emacs.d
;; Keywords: convenience, abbrev

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

;; Markdown tools.

;;; Code:

(require 'skeleton)
(require 'rx)
(require 'subr-x)

(defun lunaryorn-default-title ()
  "Get the default post title for the current buffer."
  (when (buffer-file-name)
    (let* ((fn (file-name-base (buffer-file-name)))
           (words (split-string fn "-" 'omit-nulls (rx (1+ space)))))
      (string-join (mapcar #'capitalize words) " "))))

;;;###autoload
(define-skeleton lunaryorn-markdown-post-header
  "Insert a header for blog posts."
  (read-from-minibuffer "Title: " (lunaryorn-default-title))
  "---\n"
  "title: " str "\n"
  "tags: " ("Tag: " str ",") & -1 "\n"
  "published: " (format-time-string "%F") "\n"
  "---\n\n"
  -)

(provide 'lunaryorn-markdown)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; lunaryorn-markdown.el ends here
