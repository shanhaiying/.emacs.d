;;; prelude-ui.el --- Stante Pede user interface configuration
;;
;; Copyright (c) 2012 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/stantepede.git
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

;; This module improves the general Emacs user interface.

;; Default fonts
;; -------------
;;
;; The default face of Emacs is modified to use the platform default font, if
;; possible: On OS X, this is Menlo at 13pt, on Windows Consolas at 10pt (same
;; as in Visual Studio for the sake of consistency). On other systems Dejavu
;; Sans Mono at 10pt is used.

;; Noise reduction
;; ---------------
;;
;; This module reduces the user interface noise of Emacs.  It disables the tool
;; bar and the menu bar, the startup screen, the blinking cursor and the alarm
;; beeps.  Yes/No questions are simplified to y/n questions. Frame fringes are
;; reduced to occupy less screen space.

;; More information
;; ----------------
;;
;; The user interface is also improved to provide more information.  This module
;; sets a reasonable frame title, and configures the mode line to show the
;; current line and column number, as well as an indication of the size of the
;; current buffer.
;;
;; Buffer names are modified to avoid naming collisions, using the uniquify
;; feature of Emacs.

;; Completion
;; ----------
;;
;; This module configures ido - interactive do - to improve completion when
;; switching between buffers or visiting files.  It also enables icomplete mode
;; to improve the minibuffer completion.


;;; Code:

;; Disable toolbar and menu bar (except on OS X where the menubar is present
;; anyway)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(unless (eq system-type 'darwin)
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)))

;; Disable blinking cursor
(blink-cursor-mode -1)

;; Disable alarm beeps
(setq ring-bell-function 'ignore)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Smoother scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Improve mode line
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Reduce fringes
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

;; Unify Yes/No questions
(fset 'yes-or-no-p 'y-or-n-p)

;; Show file name or buffer name in frame title
(setq frame-title-format
      '(:eval (if (buffer-file-name)
		  (abbreviate-file-name (buffer-file-name)) "%b")))

;; De-duplicate buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      ;; Re-uniquify after killing buffers
      uniquify-after-kill-buffer-p t
      ;; Ignore special buffers
      uniquify-ignore-buffers-re "^\\*")

;; Improve completion for file and buffer names
(ido-mode t)
(setq
 ;; Match characters if string doesn't match
 ido-enable-flex-matching t
 ;; Create a new buffer if absolutely nothing matches
 ido-create-new-buffer 'always
 ;; Start with filename at point if any can be guessed
 ido-use-filename-at-point 'guess
 ;; Remember ido state
 ido-save-directory-list-file (concat stante-var-dir "ido.hist")
 ;; When opening a new file, do so in the previously selected window
 ido-default-file-method 'selected-window)
;; Move summary and "output" (i.e. from Auctex) to the end to keep these out of
;; the way
(add-hook 'ido-make-buffer-list-hook 'ido-summary-buffers-to-end)

;; Improve minibuffer completion
(icomplete-mode +1)

;; Move between windows with Shift + Arrows
(require 'windmove)
(windmove-default-keybindings)

;; Default font
(cond
 ((eq system-type 'darwin)
  ;; OS X default font, but larger font size
  (set-face-attribute 'default nil :family "Menlo" :height 130))
 ((eq system-type 'windows-nt)
  ;; Visual Studio defaults
  (set-face-attribute 'default nil :family "Consolas" :height 10))
 (t
  ;; A reasonable choice for all other systems
  (set-face-attribute 'default nil :family "Dejavu Sans Mono" :height 100))
)

;; Reuse current frame for EDiff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(provide 'stante-ui)

;;; stante-ui.el ends here
