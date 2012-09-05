;;; stante-osx.el --- Stante Pede: OS X support
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

;; Paths
;; -----
;;
;; Fix paths on OS X.
;;
;; Add common directories with executables as well as files from /etc/paths.d to
;; $PATH and `exec-path'.


;;; Code:

;; For `remove-duplicates'
(require 'cl)

;; Make this module a no-op if not on OS X.
(eval-after-load 'ns-win
  '(progn
     ;; Setup modifier maps for OS X
     (setq mac-option-modifier 'meta
           mac-command-modifier 'meta
           mac-function-modifier 'control
           mac-right-option-modifier 'none
           mac-right-command-modifier 'super)

     (defconst stante-default-osx-paths
       '("/usr/local/bin" "/usr/local/sbin"
         "/usr/bin" "/usr/sbin"
         "/bin" "/sbin"))

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

     (defun stante-fix-osx-paths ()
       "Fix $PATH and `exec-path' on OS X."
       (interactive)
       (let*
           ((paths (concatenate 'list (stante-osx-paths) exec-path))
            (unique-paths (remove-duplicates paths
                                             :test 'string-equal
                                             :from-end t)))
         (setenv "PATH" (mapconcat 'identity unique-paths ":"))
         (setq exec-path unique-paths)))

     ;; Fixup paths
     (stante-fix-osx-paths)
     ))

(provide 'stante-osx)

;;; stante-osx.el ends here
