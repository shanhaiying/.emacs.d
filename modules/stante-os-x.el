;;; stante-os-x.el --- Stante Pede Modules: OS X support
;;; -*- coding: utf-8; lexical-binding: t -*-
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

;; Perform OS X specific setup.  Do nothing, if not running on OS X.

;; Modifier keys
;; -------------
;;
;; Map OS X modifier keys to Emacs modifiers in a reasonable way:
;;
;; Map the left Option key to Meta.
;;
;; Map the left Command key to Meta, too, to preserve a consistent keyboard
;; "feeling" accross various keyboards, and to prevent some bad typing mistakes
;; like accidentally closing Emacs with Command-Q while trying to fill a
;; paragraph with Meta+Q.
;;
;; Map the right Command key to Super.
;;
;; Unmap the right Option key to allow input of accented characters and Unicode
;; symbols.  On various non-US layouts some important characters are only
;; available via Option key, for instance [,],|,@ and others on a German
;; keyboard).
;;
;; Map the Function key to Control to prevent typing mistakes.  Typically this
;; key is the outermost left key, and can easily be pressed accidentally if
;; trying to reach the Control key.

;; Full screen support
;; -------------------
;;
;; Officially GNU Emacs does not support the full screen feature of OS X 10.7
;; and later.  Various patches exist to add this functionality.  Either build
;; Emacs manually with any of these patches applied, or install Emacs via
;; homebrew: "brew install Emacs --cocoa".  Note that the patch included in
;; homebrew does not create a separate space for a full screen Emacs.

;; Paths
;; -----
;;
;; Fix paths on OS X.
;;
;; Add common directories with executables as well as files from /etc/paths.d to
;; $PATH and `exec-path'.

;; Keybindings
;; -----------
;;
;; S-Enter (right option key + enter) toggles full screen mode, or shows a
;; warning message if full screen mode is not supported.


;;; Code:

(require 'stante-lib-autoloads)
(require 'cl)

;; Make this module a no-op if not on OS X GUI.
(eval-after-load 'ns-win
  #'(progn
      ;; Setup modifier maps for OS X
      (setq mac-option-modifier 'meta
            mac-command-modifier 'meta
            mac-function-modifier 'control
            mac-right-option-modifier 'none
            mac-right-command-modifier 'super)

      (unless (fboundp 'ns-toggle-fullscreen)
        (defun ns-toggle-fullscreen ()
          "Dummy for OS X fullscreen functionality."
          (message "Your Emacs build does not provide fullscreen functionality.
Install Emacs from homebrew with \"brew install emacs --cocoa\".")))

      (global-set-key (kbd "<s-return>") 'ns-toggle-fullscreen)

      ;; We don't need to fix paths for terminal emacs sessions, because these
      ;; will inherit the correct path from the shell which is hopefully
      ;; correct.  Hence we can also depend on `ns-win' for this feature.
      (stante-fix-os-x-paths)
      ))

(provide 'stante-os-x)

;;; stante-os-x.el ends here
