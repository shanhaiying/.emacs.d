;;; stante-editor.el --- Stante Pede Modules: Basic editing
;;
;; Copyright (c) 2012 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stante-pede.git
;; Keywords: abbrev convenience wp

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
;; Setup Return key to indent automatically.

;; Filling
;; -------
;;
;; Enable `auto-fill-mode' in all text modes and increase the default fill
;; column to 80 characters, which is the maximum recommended by most programming
;; style guides.

;; Guru editing
;; ------------
;;
;; Disable some common key bindings from other Editors, e.g. arrow keys, and
;; suggest alternatives to use Emacs as its meant to be used.

;; Parenthesis, brackets, etc.
;; --------------------------
;;
;; Automatically insert matching parenthesis, and highlight matching
;; parenthesis.

;; Highlighting
;; ------------
;;
;; Highlight the current line (see `global-hl-line-mode') and the results of
;; some text operations like yanking, killing and deleting (see
;; `volatile-highlights-mode').

;; Region
;; ------
;;
;; Expand the region by semantic units with `er/expand-region'.

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

;; Expansion
;; ---------
;;
;; Provide powerful word expansion with a reasonable `hippie-expand'
;; configuration.

;; Emacs server
;; ------------
;;
;; Start an Emacs server, if there is none running yet.
;;
;; This allows to edit files within the currently running Emacs session with
;; "emacsclient".

;; Keybindings
;; -----------
;;
;; C-c o shows matching lines in a new window via `occur'.
;;
;; C-x j joins this line with the preceding line and cleans up whitespace via
;; `join-line'.
;;
;; M-/ dynamically expands the word under point with `hippie-expand'.
;;
;; M-Z zaps up to, but not including a specified character, similar to
;; `zap-to-char` (see `zap-up-to-char`).
;;
;; M-x w or C-= expands the current region with the closest surrounding semantic
;; unit (see `er/expand-region').
;;
;; Return inserts a new line and indents according to mode (see
;; `newline-and-indent').


;;; Code:

(require 'stante-lib-autoloads)

;; Move backup and autosave files to var directory.
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backup" stante-var-dir)))
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save" stante-var-dir) t)))

;; Sane coding system choice
(prefer-coding-system 'utf-8)

;; Automatically revert buffers from changed files
(global-auto-revert-mode 1)

;; No tabs for indentation
(setq-default indent-tabs-mode nil
              tab-width 8)

;; Configure filling
(setq-default fill-column 80)
(setq whitespace-line-column nil)

;; Power up parenthesis
(electric-pair-mode 1)
(show-paren-mode 1)
;; Highlight only the parenthesis if it is visible, or the expression if not
(after 'paren (setq show-paren-style 'mixed))

;; Highlights
(global-hl-line-mode 1)
(package-need 'volatile-highlights)
(require 'volatile-highlights)
(volatile-highlights-mode t)
(after 'whitespace
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

;; Guru editing
(package-need 'guru-mode)
(guru-global-mode 1)

;; Cleanup stale buffers
(require 'midnight)

;; Region expansion
(package-need 'expand-region)

;; Narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Remember various histories
;; Minibuffer history
(after 'savehist
  (setq savehist-save-minibuffer-history t
        ;; Save every three minutes (the default five minutes is a bit long)
        savehist-autosave-interval 180
        ;; Move save file into proper directory
        savehist-file (expand-file-name "savehist" stante-var-dir)))
(savehist-mode t)
;; Recent files
(after 'recentf
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        ;; Move to property directory
        recentf-save-file (expand-file-name "recentf" stante-var-dir)))
(recentf-mode t)
;; Locations in files
(after 'saveplace
  (setq save-place-file (expand-file-name "saveplace" stante-var-dir))
  (setq-default save-place t))
(require 'saveplace)

;; Configure bookmarks
(after 'bookmark
  (setq bookmark-default-file (expand-file-name "bookmarks" stante-var-dir)
        ;; Save on every modification
        bookmark-save-flag 1))

;; Expansion functions
(after 'hippie-exp
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
          try-complete-lisp-symbol)))

;; Bring up Emacs server
(require 'server)
(unless (server-running-p) (server-start))

;; Flymake reloaded :)
(package-need 'flycheck)
(add-hook 'find-file-hook 'flycheck-mode)

;; Update copyright lines automatically
(add-hook 'find-file-hook 'copyright-update)

;; Some missing autoloads
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

;; Keybindings
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x j") 'join-line)
(global-set-key (kbd "M-Z") 'zap-up-to-char)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-=") 'er/expand-region) ; As suggested by documentation
(global-set-key (kbd "C-x w") 'er/expand-region)

(provide 'stante-editor)

;;; stante-editor.el ends here
