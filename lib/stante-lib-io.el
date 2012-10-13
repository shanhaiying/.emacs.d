;;; stante-lib-io.el --- Stante Pede Library: IO functions
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

;; IO functions.

;; `stante-get-file-contents' gets the contents of a file as string.
;;
;; `stante-set-file-contents' sets the contents of a file.
;;
;; `stante-get-file-lines' gets the lines in a file as list.

;; Load `stante-lib-autoloads' to use the functions of this library.

;;; Code:

;;;###autoload
(defun stante-get-file-contents (filename)
  "Return the contents of the file FILENAME."
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun stante-set-file-contents (filename contents)
  "Set the contents of the file FILENAME.

Create the file FILENAME if it does not exist, or completely
overwrite it if it does."
  (with-temp-buffer
    (insert contents)
    (write-region (point-min) (point-max) filename nil 0)))

;;;###autoload
(defun stante-get-file-lines (filename)
  "Return a list of lines of file FILENAME."
  (split-string (stante-get-file-contents filename) "\n" t))

(provide 'stante-lib-io)

;;; stante-lib-io.el ends here
