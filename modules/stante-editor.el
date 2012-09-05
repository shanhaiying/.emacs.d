;;; stante-editor.el --- Stante Pede: Configuration for general editing
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

;; Setup editing.

;; Indentation and tabs
;; --------------------
;;
;; The default `tab-width' is set to 8, but indentation with tabs is disabled.

;; Filling
;; -------
;;
;; Auto filling is enabled in all text modes, and the default fill column is
;; increased to 80 characters.

;; Parenthesis, brackets, etc.
;; --------------------------
;;
;; Electric pairing and highlighting of matching parens are enabled.

;; Highlighting
;; ------------
;;
;; The current line is highlighted, and `volatile-highlights-mode' is enabled.
;; This mode highlights the results of some text operations, like yanking,
;; killing or deleting.

;; Narrowing
;; ---------
;;
;; Narrowing commands are enabled, so C-x n n, C-x n d and C-x n p are available.

;; History
;; -------
;;
;; Minibuffer history, recent files and location in files are saved and restored.

;; Keybindings
;; -----------
;;
;; C-x j joins this line with the preceding line and cleans up whitespace via
;; `join-line'.
;;
;; M-/ does `hippie-expand', replacing the much less powerfull `dabbrev-expand'.


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
