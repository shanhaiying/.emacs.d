;;; stante-editor.el --- Stante Pede: Configure for general editing
;;
;; Copyright (c) 2012 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stantepede.git
;; Version: 1.0.0
;; Keywords: convenience

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

;; Configure general editing.

;; Indentation and tabs
;; --------------------
;;
;; Set the default `tab-width' is set to 8, but disable indentation with tabs.

;; Filling
;; -------
;;
;; Enable `auto-fill-mode' in all text modes and increase the default fill
;; column to 80 characters, which is the maximum recommended by most programming
;; style guides.

;; Parenthesis, brackets, etc.
;; --------------------------
;;
;; Automatically insert matching parenthesis, and highlight matching parenthesis.

;; Highlighting
;; ------------
;;
;; Highlight the current line (see `global-hl-line-mode') and the results of
;; some text operations like yanking, killing and deleting (see
;; `volatile-highlights-mode').

;; Narrowing and widening
;; ----------------------
;;
;; Enable narrowing commands `narrow-to-region' (C-x n n), `narrow-to-page' (C-x
;; n p) and `narrow-to-defun' (C-x n d).  These commands reduce the visible text
;; to the current region (or page or defun, respectively).  Use `widen' (C-x n
;; w) to remove narrowing.

;; History
;; -------
;;
;; Save and restore minibuffer history, recent files and the location of point
;; in files.

;; Keybindings
;; -----------
;;
;; C-x j joins this line with the preceding line and cleans up whitespace via
;; `join-line'.
;;
;; M-/ dynamically expands the word under point with `hippie-expand'.

;; Code:
(require 'stante-helper)

;; Move backup and autosave files to var directory.
(setq
 backup-directory-alist `((".*" . ,(concat stante-var-dir "backup")))
 auto-save-file-name-transforms `((".*" ,(concat stante-var-dir "auto-save") t)))

;; No tabs for indentation
(setq-default indent-tabs-mode nil
              tab-width 8)

;; Configure filling
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Power up parenthesis
(electric-pair-mode 1)
(show-paren-mode 1)
;; Highlight only the parenthesis if it is visible, or the expression if not
(setq show-paren-style 'mixed)

;; Highlights
(global-hl-line-mode 1)
(package-install-if-needed 'volatile-highlights)
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Cleanup stale buffers
(require 'midnight)

;; Narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Remember various histories
;; Minibuffer history
(setq savehist-save-minibuffer-history t
      ;; Save every three minutes (the default five minutes is a bit long)
      savehist-autosave-interval 180
      ;; Move save file into proper directory
      savehist-file (concat stante-var-dir "savehist"))
(savehist-mode t)
;; Recent files
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15
      ;; Move to property directory
      recentf-save-file (concat stante-var-dir "recentf"))
(recentf-mode t)
;; Locations in files
(setq save-place-file (concat stante-var-dir "saveplace"))
(setq-default save-place t)
(require 'saveplace)

;; Configure bookmarks
(setq bookmark-default-file (concat stante-var-dir "bookmarks")
      ;; Save on every modification
      bookmark-save-flag 1)

;; Expansion functions
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))


;; Keybindings
(global-set-key (kbd "C-x j") 'join-line)
(global-set-key (kbd "M-/") 'hippie-expand)

(provide 'stante-editor)

;;; stante-editor.el ends here
