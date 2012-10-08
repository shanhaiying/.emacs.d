;;; prelude-ui.el --- Stante Pede Modules: User interface configuration
;;; -*- coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (c) 2012 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/stante-pede.git
;; Keywords: convenience frames tools

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

;; Improve the general user interface of Emacs.

;; Default fonts
;; -------------
;;
;; Set the default font.
;;
;; On OS X, use Menlo 13pt.
;;
;; On Windows, use Consolas 10pt, matching the Visual Studio defaults.  Consolas
;; is not available by default, but comes with many Microsoft programs,
;; including Visual Studio, Office, and others.
;;
;; On all other systems, use Dejavu Sans Mono 10pt.  This font is used as
;; standard monospace font on many Linux distributions, hence seems a good
;; choice for Stante Pede, too.

;; Noise reduction
;; ---------------
;;
;; Reduce the user interface noise of Emacs.
;;
;; Disable the tool bar, the menu bar (except on OS X where the menu bar is
;; always present), the startup screen, the blinking cursor and the alarm beeps.
;; Simplify Yes/No questions to y/n, and reduce frame fringes to occupy less
;; screen space.

;; More information
;; ----------------
;;
;; Add more information to the user interface.
;;
;; Set a reasonable frame title and configure the mode line to show the current
;; line and column number, as well as an indication of the size of the current
;; buffer.
;;
;; Uniquify buffer names in case of naming collisions.

;; Completion
;; ----------
;;
;; Enable completion via ido – interactive do – for buffer switching and file
;; visiting.  Enable icomplete mode to improve minibuffer completion.

;; Initial frame position and size
;; -------------------------------
;;
;; Save the position and size of the current frame when killing Emacs and apply
;; these saved parameters to the initial frame.  If Emacs is with a single
;; frame, effectively remembers the frame position and size like other GUI
;; applications.

;; Key bindings
;; ------------
;;
;; C-x C-b shows IBuffer (see  `ibuffer').  Replaces the standard `buffer-menu'.
;;
;; C-x p shows a list of running processes similar to the Unix command line
;; utility "top".
;;
;; C-h A searches for any Lisp symbol matching a regular expression (see
;; `apropos').


;;; Code:

(require 'stante-lib-autoloads)

;; Disable toolbar and menu bar (except on OS X where the menubar is present
;; anyway)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(unless (stante-is-os-x)
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
      uniquify-after-kill-buffer-p t       ; Re-uniquify after killing buffers
      uniquify-ignore-buffers-re "^\\*")   ; Ignore special buffers


;; Improve completion for file and buffer names
(ido-mode t)
(setq ido-enable-flex-matching t    ; Match characters if string doesn't match
      ido-create-new-buffer 'always ; Create a new buffer if nothing matches
      ido-use-filename-at-point 'guess
      ido-save-directory-list-file (concat stante-var-dir "ido.hist")
      ido-default-file-method 'selected-window)

;; Move summary and "output" (i.e. from Auctex) to the end to keep these out
;; of the way
(add-hook 'ido-make-buffer-list-hook 'ido-summary-buffers-to-end)


;; Improve minibuffer completion
(icomplete-mode +1)

;; Move between windows with Shift + Arrows
(require 'windmove)
(windmove-default-keybindings)

;; Default font
(cond
 ((stante-is-os-x)
  ;; OS X default font, but larger font size
  (set-face-attribute 'default nil :family "Menlo" :height 130))
 ((stante-is-windows)
  ;; Visual Studio defaults
  (set-face-attribute 'default nil :family "Consolas" :height 100))
 (t
  ;; A reasonable choice for all other systems
  (set-face-attribute 'default nil :family "Dejavu Sans Mono" :height 100))
 )

;; Reuse current frame for EDiff
(after 'ediff-wind
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; Theme support
(defvar stante-known-themes-alist
  '((birds-of-paradise-plus . birds-of-paradise-plus-theme)
    (inkpot . inkpot-theme)
    (ir-black . ir-black-theme)
    (molokai . molokai-theme)
    (pastels-on-dark . pastels-on-dark-theme)
    (solarized-light . color-theme-solarized)
    (solarized-dark . color-theme-solarized)
    (tango-2 . tango-2-theme)
    (twilight-anti-bright . twilight-anti-bright-theme)
    (twilight-bright . twilight-bright-theme)
    (twilight . twilight-theme)
    (zen-and-art . zen-and-art-theme)
    (zenburn . zenburn-theme))
  "Color themes know to stante.

Maps the theme name to the package that contains this theme.
This is is naturally incomplete.  Feel free to extend, and please
report color themes not contained in this list to
https://github.com/lunaryorn/stante-pede/issues.")

(defadvice load-theme (before load-theme-install-package activate compile)
  "Ensure that the package containing the theme is installed.

If a package containing the theme is known, but not installed, it
is automatically installed before the theme is loaded.  See
`stante-known-themes-alist' for a list of known theme names and
corresponding packages."
  (let* ((theme (ad-get-arg 0))
         (package (cdr (assoc theme stante-known-themes-alist))))
    (when package
      (message "Installing package %s for theme %s." package theme)
      (package-need package))))

(defvar stante-save-frame-parameters-file
  (concat stante-var-dir "frame-parameters")
  "File in which to storce frame parameters on exit.")

(defun stante-restore-frame-parameters ()
  "Restore the frame parameters of the initial frame."
  (condition-case nil
      (let* ((params (read (stante-get-file-contents
                            stante-save-frame-parameters-file))))
        ;; Verify the read expression
        (when (and (listp params) (eq (length params) 4))
          (setq initial-frame-alist
                (stante-merge-alists initial-frame-alist params))))
    (error nil)))

(defun stante-save-frame-parameters ()
  "Save frame parameters of the selected frame.

Save the top left position and the width and height to
`stante-save-frame-parameters-file'."
  (condition-case nil
      (let ((params (frame-parameters)))
        (when (and params (display-graphic-p)) ; GUI frames only!
          (let ((extends (list (assq 'left params)
                               (assq 'top params)
                               (assq 'width params)
                               (assq 'height params))))
            (stante-set-file-contents
             stante-save-frame-parameters-file
             (with-output-to-string (prin1 extends) (terpri)))
            t)))
    (file-error nil)))
(unless noninteractive
       (add-hook 'after-init-hook 'stante-restore-frame-parameters)
       (add-hook 'kill-emacs-hook 'stante-save-frame-parameters))

;; Key bindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Similar to C-x d
(global-set-key (kbd "C-x p") 'proced)
;; Complementary to C-h a
(global-set-key (kbd "C-h A") 'apropos)

(provide 'stante-ui)

;;; stante-ui.el ends here
