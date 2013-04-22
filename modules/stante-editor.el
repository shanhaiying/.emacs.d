;;; stante-editor.el --- Stante Pede Modules: Basic editing -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012, 2013 Sebastian Wiesner
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
;;
;; Indicate empty lines at the end of the file.

;; Region
;; ------
;;
;; Expand the region by semantic units with `er/expand-region'.
;;
;; Wrap the region with Wrap Region Mode.

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
;; C-<backspace> kills a line backwards and re-indents.
;;
;; C-S-<backspace> kills a whole line and moves back to indentation.
;;
;; S-return or C-S-j insert a new empty line below the current one.
;;
;; C-c o shows matching lines in a new window via `occur'.
;;
;; M-/ dynamically expands the word under point with `hippie-expand'.
;;
;; M-Z zaps up to, but not including a specified character, similar to
;; `zap-to-char` (see `zap-up-to-char`).
;;
;; C-= expands the current region with the closest surrounding semantic unit
;; (see `er/expand-region').
;;
;; C-c SPC starts Ace Jump mode to quickly navigate in the buffer.  C-x SPC
;; jumps back.


;;; Code:

(eval-when-compile
  (require 'drag-stuff)
  (require 'whitespace)
  (require 'paren)
  (require 'electric)
  (require 'savehist)
  (require 'recentf)
  (require 'saveplace)
  (require 'bookmark))

;; Move backup and autosave files to var directory.
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backup" stante-var-dir)))
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save" stante-var-dir) t)))

;; Sane coding system choice
(prefer-coding-system 'utf-8)

;; Automatically revert buffers from changed files
(global-auto-revert-mode 1)

;; Delete selection when entering new text
(delete-selection-mode)

;; Wrap the region
(wrap-region-mode)

;; View readonly files
(setq view-read-only t)

;; Nice page breaks
(global-page-break-lines-mode)
(after 'page-break-lines (diminish 'page-break-lines-mode))

;; No tabs for indentation
(setq-default indent-tabs-mode nil
              tab-width 8)
;; Automatic indentation
(electric-indent-mode 1)
(electric-layout-mode 1)

;; Drag stuff around
(drag-stuff-global-mode)
(after 'drag-stuff
  (setq drag-stuff-modifier '(meta shift))
  (diminish 'drag-stuff-mode))

(defun stante-editor-disable-electric-indentation ()
  "Disable electric indentation."
  (set (make-local-variable 'electric-indent-functions)
       (list (lambda (_arg) 'no-indent))))

;; Configure filling
(setq-default fill-column 80)
(after 'whitespace
  (setq whitespace-line-column nil))

;; Configure wrapping
(add-hook 'text-mode-hook 'adaptive-wrap-prefix-mode)

;; Power up parenthesis
(electric-pair-mode 1)
(show-paren-mode 1)
;; Highlight only the parenthesis if it is visible, or the expression if not
(after 'paren (setq show-paren-style 'mixed))

;; Highlights
(global-hl-line-mode 1)
(require 'volatile-highlights)          ; Volatile Highlights doesn't autoload
(volatile-highlights-mode t)
(after 'volatile-highlights (diminish 'volatile-highlights-mode))

(after 'whitespace
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(setq indicate-empty-lines t)

;; Cleanup stale buffers
(require 'midnight)

;; Narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Remember minibuffer history
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

;; Shamelessly stolen from
;; http://emacsredux.com/blog/2013/04/05/recently-visited-files/
(defun recentf-ido-find-file ()
  "Find a recent file with Ido."
  (interactive)
  (let ((file (ido-completing-read "Find recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; Remember locations in files
(after 'saveplace
  (setq save-place-file (expand-file-name "saveplace" stante-var-dir))
  (setq-default save-place t))
(require 'saveplace)

;; Configure bookmarks
(after 'bookmark
  (setq bookmark-default-file (expand-file-name "bookmarks" stante-var-dir)
        ;; Save on every modification
        bookmark-save-flag 1))

;; Completion
(setq completion-cycle-threshold 5)     ; Cycle with less than 5 candidates

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
(global-flycheck-mode)

;; Update copyright lines automatically
(add-hook 'find-file-hook 'copyright-update)

;; Some missing autoloads
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

;; Keybindings
(global-set-key (kbd "C-<backspace>") 'stante-smart-backward-kill-line)
(global-set-key [remap kill-whole-line] 'stante-smart-kill-whole-line)
(global-set-key (kbd "C-S-j") 'stante-smart-open-line)
(global-set-key [(shift return)] 'stante-smart-open-line)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "M-Z") 'zap-up-to-char)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-=") 'er/expand-region) ; As suggested by documentation
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(global-set-key (kbd "C-c f") 'recentf-ido-find-file)

(provide 'stante-editor)

;; Local Variables:
;; coding: utf-8
;; End:

;;; stante-editor.el ends here
