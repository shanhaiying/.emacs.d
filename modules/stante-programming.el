;;; stante-programming.el --- Stante Pede: Basic programming support
;;; -*- coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (c) 2012 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stantepede.git
;; Version: 1.0.0
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

;; Setup programming modes.

;; This module is automatically loaded by Stante Pede modules that configure
;; specific programming modes so you should normally not need to enable it
;; explicitly.  However, if you are using a programming mode that has no
;; specific Stante Pede module, you might want to load this module to enable
;; some basic tweaks.  In this case, please consider writing a proper Stante
;; Pede module for this mode and contribute it to the Stante Pede project.

;; A programming mode in terms of this module is a mode that is derived from
;; `prog-mode'.  In Emacs 24, most programming modes should do so, but 3rd party
;; packages might not.  In such cases, most functionality of this module will
;; not be enabled.  If Stante Pede provides a module for such a mode, enable
;; this module to get the functionality of this module.

;; Filling
;; -------
;;
;; Automatically fill comments in each programming buffer.

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
;; `stante-whitespace-mode' enables `whitespace-mode' for the current buffer and
;; arranges for whitespace to be cleaned up before the current buffer is saved
;; (see `whitespace-cleanup').
;;
;; `stante-programming-keybindings' configures the keybindings of this module
;; for the current local mode map.
;;
;; `stante-configure-programming-mode-hook' sets up a given hook with all of the
;; above functions, simply for convenience.

;; Keybindings
;; -----------
;;
;; C-# toggles commenting for current region (see
;; `comment-or-uncomment-region').


;;; Code:

;; Default semantic submodes
(setq semantic-default-submodes
      '(global-semanticdb-minor-mode ;; Cache database
        global-semantic-idle-scheduler-mode ;; Re-parse when idle
        global-semantic-idle-summary-mode ;; Show tab summary when idle
        global-semantic-stickyfunc-mode ;; Show current tag at top of buffer
        ))
;; Move semantic database to proper place
(setq semanticdb-default-save-directory (concat stante-var-dir "semanticdb"))

(eval-after-load 'simple ;; prog-mode is contained in simple.el
  #'(progn

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

      (defun stante-whitespace-mode ()
        "Enable whitespace mode for the current buffer and cleanup whitespace
       before saving the current buffer."
        (whitespace-mode 1)
        (add-hook 'before-save-hook 'whitespace-cleanup nil t))

      (defun stante-programming-keybindings ()
        "Add the keybindings of this module to the `current-local-map'."
        (local-set-key (kbd "C-#") 'comment-or-uncomment-region))

      (defun stante-setup-programming-mode-hook (hook)
        "Add all local programming setup functions to HOOK.

Currently this functions adds `stante-auto-fill-comments',
`stante-whitespace-mode', `stante-programming-keybindings' and
`stante-add-task-keywords' to HOOK."
        (dolist (func '(stante-auto-fill-comments
                        stante-whitespace-mode
                        stante-programming-keybindings
                        stante-add-task-keywords))
          (add-hook hook func)))

      (stante-setup-programming-mode-hook 'prog-mode-hook)

      ;; Enable semantic mode
      (semantic-mode 1)
      ))

(provide 'stante-programming)

;;; stante-programming.el ends here
