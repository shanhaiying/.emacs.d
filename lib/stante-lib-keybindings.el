;;; stante-lib-keybindings.el --- Stante Pede Library: Keybindings -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2013 Sebastian Wiesner
;; Copyright (c) 2011-2013 Bozhidar Batsov
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

;; Key bindings for multiple cursors

;;; Code:

(defvar stante-keybindings-mode-map
  (let ((map (make-sparse-keymap))
        (pmap (make-sparse-keymap)))
    (define-key pmap "l" #'mc/edit-lines)
    (define-key pmap (kbd "C-a") #'mc/edit-beginnings-of-lines)
    (define-key pmap (kbd "C-e") #'mc/edit-ends-of-lines)
    (define-key pmap (kbd "C-s") #'mc/mark-all-in-region)
    (define-key pmap ">" #'mc/mark-next-like-this)
    (define-key pmap "<" #'mc/mark-previous-like-this)
    (define-key pmap "e" #'mc/mark-more-like-this-extended)
    (define-key pmap "h" #'mc/mark-all-like-this-dwim)
    (define-key map (kbd "C-c m") pmap)
    map)
  "Key map for multiple cursors.")

(defvar stante-file-commands-mode-map
  (let ((map (make-sparse-keymap))
        (pmap (make-sparse-keymap)))
    (define-key pmap "r" #'stante-ido-find-recentf)
    (define-key pmap "o" #'stante-open-with)
    (define-key pmap "R" #'stante-rename-file-and-buffer)
    (define-key pmap "D" #'stante-delete-file-and-buffer)
    (define-key pmap "w" #'stante-copy-filename-as-kill)
    (define-key map (kbd "C-c f") pmap)
    map)
  "Key map for file functions.")

(defvar stante-symbol-mode-map
  (let ((map (make-sparse-keymap))
        (pmap (make-sparse-keymap)))
    (define-key pmap "o" #'highlight-symbol-occur)
    (define-key pmap "%" #'highlight-symbol-query-replace)
    (define-key pmap "n" #'highlight-symbol-next-in-defun)
    (define-key pmap "p" #'highlight-symbol-prev-in-defun)
    (define-key pmap (kbd "M-n") #'highlight-symbol-next)
    (define-key pmap (kbd "M-p") #'highlight-symbol-prev)
    (define-key map (kbd "C-c s") pmap)
    map)
  "Key map to work on symbols.")

;;;###autoload
(define-minor-mode stante-file-commands-mode
  "A global minor mode to provide file commands.

\\{stante-file-commands-mode-map}"
  :global t
  :lighter nil)

;;;###autoload
(define-minor-mode stante-multiple-cursors-mode
  "A global minor mode for Multiple Cursor keybindings.

\\{stante-multiple-cursors-mode-map}"
  :global t
  :lighter nil)

;;;###autoload
(define-minor-mode stante-symbol-mode
  "A minor mode for keybindings working on symbols.

\\{stante-symbol-mode-map}"
  :lighter nil)

(provide 'stante-lib-keybindings)

;; Local Variables:
;; coding: utf-8
;; End:

;;; stante-lib-keybindings.el ends here
