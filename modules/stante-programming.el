;;; stante-programming.el --- Stante Pede Modules: Basic programming support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012, 2013 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stante-pede.git
;; Keywords: convenience languages tools wp

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

;; Setup programming modes based on `prog-mode'.

;; This module is automatically loaded by Stante Pede modules that configure
;; specific programming modes.  You do not need to enable it explicitly.

;; A programming mode in terms of this module is a mode that is derived from
;; `prog-mode'.  In Emacs 24, most programming modes should do so, but 3rd party
;; packages might not.  In such cases, most functionality of this module will
;; not be enabled.  If Stante Pede provides a module for such a mode, enable
;; this module to get the functionality of this module.  You'll also need to run
;; `prog-mode-hook' manually in the mode hook.

;; Filling
;; -------
;;
;; Automatically fill comments in each programming buffer.

;; Guru editing
;; ------------
;;
;; Disable some common key bindings from other Editors, e.g. arrow keys, and
;; suggest alternatives to use Emacs as its meant to be used.

;; Task keywords
;; -------------
;;
;; Highlight tasks marked by TODO:, FIXME:, FIX: and HACK: keywords with
;; `font-lock-warning-face'.

;; Whitespace
;; ----------
;;
;; Highlight whitespace in programming buffers (see `whitespace-mode'), and
;; clean whitespace before saving (see `whitespace-cleanup').

;; Semantic parsing
;; ----------------
;;
;; Parse buffers with `semantic-mode'.
;;
;; Show summary for tags when idle, and keep the current function name sticky on
;; top of the buffer.

;; Symbol highlighting
;; -------------------
;;
;; Highlight the symbol under point.

;; Reusability
;; -----------
;;
;; Buffer local settings employed by this module are enabled through
;; `prog-mode-hook'.  If a programming mode does *not* derive from `prog-mode'
;; it does not run this hook and hence does not enable Stante Pede programming
;; settings.
;;
;; If you are writing a Stante Pede module for a progamming mode that does not
;; derive from `prog-mode' you can use the functions provided by this module to
;; configure the hook of this mode.
;;
;; `stante-add-task-keywords' adds warning highlighting for FIX, FIXME, TODO and
;; HACK comments for the current buffer.
;;
;; `stante-auto-fill-comments' enables auto-filling for comments and strings in
;; the current buffer.
;;
;; `stante-programming-whitespace' enables `whitespace-mode' for the current
;; buffer and arranges for whitespace to be cleaned up before the current buffer
;; is saved (see `whitespace-cleanup').
;;
;; `stante-programming-keybindings' configures the keybindings of this module
;; for the current local mode map.

;; Keybindings
;; -----------
;;
;; `stante-symbol-mode' provides keybindings to work on symbols.

;;; Code:

(eval-when-compile
  (require 'highlight-symbol))
(require 'dash)

;; Default semantic submodes
(setq semantic-default-submodes
      '(global-semanticdb-minor-mode ;; Cache database
        global-semantic-idle-scheduler-mode ;; Re-parse when idle
        global-semantic-idle-summary-mode ;; Show tab summary when idle
        global-semantic-stickyfunc-mode ;; Show current tag at top of buffer
        ))

;; Move semantic database to proper place
(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" stante-var-dir))

(defun stante-add-task-keywords ()
  "Highlight tasks in the current buffer.

Tasks are marked by FIX:, TODO:, FIXME: and HACK: keywords, and are highlighted
with `font-lock-warning-face'."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\):"
          1 font-lock-warning-face t))))

(defun stante-auto-fill-comments ()
  "Enable auto-filling for comments in the current buffer."
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode 1))

(defun stante-programming-whitespace ()
  "Enable whitespace mode for the current buffer.

Also arrange for a whitespace cleanup before saving."
  (whitespace-mode 1)
  (add-hook 'before-save-hook 'whitespace-cleanup nil t))

(after 'simple ; prog-mode is contained in simple.el
  (--each '(stante-auto-fill-comments
            stante-programming-whitespace
            stante-add-task-keywords
            guru-mode
            highlight-symbol-mode
            stante-symbol-mode)
    (add-hook 'prog-mode-hook it)))

(after 'highlight-symbol
  ;; Highlight the symbol under point after short delay, and highlight the
  ;; symbol immediately after symbol navigation
  (setq highlight-symbol-idle-delay 0.4
        highlight-symbol-on-navigation-p t)

  (diminish 'highlight-symbol-mode))

;; Enable semantic
(semantic-mode 1)

;; De-clutter mode line
(after 'guru-mode (diminish 'guru-mode))

(provide 'stante-programming)

;; Local Variables:
;; coding: utf-8
;; End:

;;; stante-programming.el ends here
