;;; stante-osx.el --- Stante Pede Modules: OS X support
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

(require 'cl)

(defconst stante-default-osx-paths
  '("/usr/local/bin" "/usr/local/sbin"
    "/usr/bin" "/usr/sbin"
    "/bin" "/sbin")
  "Default executable paths on OS X.")

(defun stante-osx-paths ()
  "Return a list of executable paths for OS X."
  (concatenate 'list
               stante-default-osx-paths
               (stante-read-osx-pathfiles)))

(defun stante-read-osx-pathfiles ()
  "Return a list of executable paths read from the files in /etc/paths.d."
  (condition-case nil
      (let* ((contents (directory-files "/etc/paths.d" t))
             (files (remove-if-not 'file-regular-p contents)))
        (mapcan 'stante-get-file-lines files))
    (file-error nil)))

(defun stante-find-osx-coreutils ()
  "Return the directory containing the unprefixed GNU coreutils on OS X.

If the directory cannot be determined, return nil.

Currently this function only checks for coreutils installed with
homebrew.  In future, more sophisticated logic might be added."
  (condition-case nil
      (let ((prefix (car (process-lines "brew" "--prefix" "coreutils"))))
        (concat (directory-file-name prefix) "/libexec/gnubin"))
    (error nil)))

(defun stante-fix-osx-paths ()
  "Fix $PATH and `exec-path' on OS X."
  (interactive)
  (let*
      ((paths (concatenate 'list (stante-osx-paths) exec-path))
       (unique-paths (remove-duplicates paths
                                        :test 'string=
                                        :from-end t)))
    (setenv "PATH" (mapconcat 'identity unique-paths ":"))
    (setq exec-path unique-paths)
    ;; Search coreutils *after* basic path fixing to make sure that "brew"
    ;; is in `exec-path'.
    (let ((coreutils-dir (stante-find-osx-coreutils)))
      (if coreutils-dir
          ;; Do *not* add the GNU coreutils directory to $PATH because it
          ;; must not be exported to Emacs subprocesses.  On OS X programs
          ;; might break if the call out to GNU utilities!
          (add-to-list 'exec-path coreutils-dir nil 'string=)
        (message "GNU Coreutils not found.  Install coreutils \
with homebrew, or report an issue to %s." stante-issues-url)))))

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
      (stante-fix-osx-paths)
      ))

(provide 'stante-osx)

;;; stante-osx.el ends here
