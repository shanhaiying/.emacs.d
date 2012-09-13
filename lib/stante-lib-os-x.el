;;; stante-lib-os-x.el --- Stante Pede Library: OS X support
;;; -*- coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (c) 2012 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stantepede.git
;; Version: 1.0.0
;; Keywords: extensions

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

;; OS X support functions.

;;

;; Load `stante-lib-autoloads' to use the functions of this library.

;;; Code:

(require 'cl)

(defconst stante-default-os-x-paths
  '("/usr/local/bin" "/usr/local/sbin"
    "/usr/bin" "/usr/sbin"
    "/bin" "/sbin")
  "Default executable paths on OS X.")

(defun stante-os-x-paths ()
  "Return a list of executable paths for OS X."
  (concatenate 'list
               stante-default-os-x-paths
               (stante-read-os-x-pathfiles)))

(defun stante-read-os-x-pathfiles ()
  "Return a list of executable paths read from the files in /etc/paths.d."
  (condition-case nil
      (let* ((contents (directory-files "/etc/paths.d" t))
             (files (remove-if-not 'file-regular-p contents)))
        (mapcan 'stante-get-file-lines files))
    (file-error nil)))

(defun stante-find-os-x-coreutils ()
  "Return the directory containing the unprefixed GNU coreutils on OS X.

If the directory cannot be determined, return nil.

Currently this function only checks for coreutils installed with
homebrew.  In future, more sophisticated logic might be added."
  (condition-case nil
      (let ((prefix (car (process-lines "brew" "--prefix" "coreutils"))))
        (concat (directory-file-name prefix) "/libexec/gnubin"))
    (error nil)))

;;;###autoload
(defun stante-fix-os-x-paths ()
  "Fix $PATH and `exec-path' on OS X."
  (interactive)
  (let*
      ((paths (concatenate 'list (stante-os-x-paths) exec-path))
       (unique-paths (remove-duplicates paths
                                        :test 'string=
                                        :from-end t)))
    (setenv "PATH" (mapconcat 'identity unique-paths ":"))
    (setq exec-path unique-paths)
    ;; Search coreutils *after* basic path fixing to make sure that "brew"
    ;; is in `exec-path'.
    (let ((coreutils-dir (stante-find-os-x-coreutils)))
      (if coreutils-dir
          ;; Do *not* add the GNU coreutils directory to $PATH because it
          ;; must not be exported to Emacs subprocesses.  On OS X programs
          ;; might break if the call out to GNU utilities!
          (add-to-list 'exec-path coreutils-dir nil 'string=)
        (message "GNU Coreutils not found.  Install coreutils \
with homebrew, or report an issue to %s." stante-issues-url)))))

;;; Code:

(provide 'stante-lib-os-x)

;;; stante-lib-os-x.el ends here
