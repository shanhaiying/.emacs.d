;;; stante-programming.el --- Stante Pede: Programming functions -*- lexical-binding: t; -*-
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

;; Support modes for programming modes.

;;; Code:

;;;###autoload
(define-minor-mode stante-auto-fill-comments-mode
  "Minor mode to auto-fill comments only."
  :lighter nil
  :keymap nil
  (cond
   (stante-auto-fill-comments-mode
    (set (make-local-variable 'comment-auto-fill-only-comments) t)
    (auto-fill-mode 1))
   (:else
    (kill-local-variable 'comment-auto-fill-only-comments)
    (auto-fill-mode -1))))

;;;###autoload
(define-minor-mode stante-prog-whitespace-mode
  "Minor mode to highlight and cleanup whitespace."
  :lighter nil
  :keymap nil
  (cond
   (stante-prog-whitespace-mode
    (whitespace-mode 1)
    (add-hook 'before-save-hook 'whitespace-cleanup nil :local))
   (:else
    (whitespace-mode -1)
    (remove-hook 'before-save-hook 'whitespace-cleanup :local))))

(provide 'stante-programming)

;; Local Variables:
;; coding: utf-8
;; End:

;;; stante-programming.el ends here
