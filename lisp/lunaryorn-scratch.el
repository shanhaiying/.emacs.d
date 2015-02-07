;;; lunaryorn-scratch.el --- My personal scratch buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Sebastian Wiesner

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

;; My personal scratch buffer, with a nice logo.

;;; Code:

(defconst lunaryorn-logo-file (locate-user-emacs-file "logo.png")
  "The path to my logo.")

(defconst lunaryorn-logo-url "http://www.lunaryorn.com/logo.png"
  "The URL of my logo.")

;;;###autoload
(defun lunaryorn-insert-logo ()
  "Insert my logo into the current buffer."
  (interactive)
  (unless (file-exists-p lunaryorn-logo-file)
    (url-copy-file lunaryorn-logo-url lunaryorn-logo-file
                   nil 'keep-time))
  (insert-image (create-image lunaryorn-logo-file) "logo")
  (insert "\n"))

;;;###autoload
(defun lunaryorn-insert-logo-into-scratch ()
  "Insert my logo into the scratch buffer."
  (with-current-buffer "*scratch*"
    (goto-char (point-max))
    (insert "\n")
    (lunaryorn-insert-logo)))

(provide 'lunaryorn-scratch)
;;; lunaryorn-scratch.el ends here
