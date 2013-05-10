;;; stante-emacs-lisp.el --- Stante Pede: Emacs Lisp editing functions -*- lexical-binding: t; -*-
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

;; Functions and modes for Emacs Lisp editing.

;;; Code:

(defun stante-emacs-lisp-clean-byte-code (&optional buffer)
  "Remove byte code file corresponding to the Emacs Lisp BUFFER.

BUFFER defaults to the current buffer."
  (when (eq major-mode 'emacs-lisp-mode)
    (let ((bytecode (concat  (buffer-file-name buffer) "c")))
      (when (file-exists-p bytecode)
        (delete-file bytecode)))))

;;;###autoload
(define-minor-mode stante-emacs-lisp-clean-byte-code-mode
  "Minor mode to automatically clean stale Emacs Lisp bytecode."
  :lighter nil
  :keymap nil
  (if stante-emacs-lisp-clean-byte-code-mode
      (add-hook 'after-save-hook 'stante-emacs-lisp-clean-byte-code nil :local)
    (remove-hook 'after-save-hook 'stante-emacs-lisp-clean-byte-code :local)))

;;;###autoload
(defun stante-emacs-lisp-switch-to-ielm ()
  "Switch to an ielm window.

Create a new ielm process if required."
  (interactive)
  (pop-to-buffer (get-buffer-create "*ielm*"))
  (ielm))

(provide 'stante-emacs-lisp)

;; Local Variables:
;; coding: utf-8
;; End:

;;; stante-emacs-lisp.el ends here
