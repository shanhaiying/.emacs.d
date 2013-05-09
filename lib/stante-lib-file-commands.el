;;; stante-lib-file-commands.el --- Stante Pede Library: File commands -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2013 Sebastian Wiesner
;; Copyright (c) 2011-2013 Bozhidar Batsov
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stante-pede.git
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

;; Commands for working with files.

;; The functions in this module are heavily inspired by the Emacs Redux and by
;; Emacs Prelude.  See:
;;
;; http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
;; http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
;; http://emacsredux.com/blog/2013/03/27/open-file-in-external-program/
;; http://emacsredux.com/blog/2013/04/05/recently-visited-files/
;; https://github.com/bbatsov/prelude/blob/master/core/prelude-core.el

;;; Code:

(require 'recentf)

(defun stante-get-standard-open-command ()
  "Get the standard command to open a file.

Return the command as shell command, or nil if there is no standard command
for the current platform."
  (cond
   ((eq system-type 'darwin) "open")
   ((memq system-type '(gnu gnu/linux gnu/kfreebsd)) "xdg-open")))

;;;###autoload
(defun stante-open-with (arg)
  "Open the file visited by the current buffer externally.

Use the standard program to open the file.  With prefix ARG,
prompt for the command to use."
  (interactive "P")
  (unless (buffer-file-name)
    (user-error "This buffer is not visiting a file"))
  (let ((command (unless arg (stante-get-standard-open-command))))
    (unless command
      (setq command (read-shell-command "Open current file with: ")))
    (shell-command (concat command " "
                           (shell-quote-argument (buffer-file-name))))))

;;;###autoload
(defun stante-ido-find-recentf ()
  "Find a recent file with IDO."
  (interactive)
  (let ((file (ido-completing-read "Find recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;;;###autoload
(defun stante-copy-filename-as-kill ()
  "Copy the name of the currently visited file to kill ring."
  (interactive)
  (let ((filename (if (eq major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (unless filename
      (user-error "This buffer is not visiting a file"))
    (kill-new filename)))

;;;###autoload
(defun stante-rename-file-and-buffer ()
  "Rename the current file and buffer."
  (interactive)
  (let* ((filename (buffer-file-name))
         (new-name (read-file-name "New name: " nil nil nil
                                   (or filename (buffer-name)))))
    (cond
     ((not (and filename (file-exists-p filename))) (rename-buffer new-name))
     ((vc-backend filename) (vc-rename-file filename new-name))
     (:else
      (rename-file filename new-name :force-overwrite)
      (set-visited-file-name new-name :no-query :along-with-file)))))

;;;###autoload
(defun stante-delete-file-and-buffer ()
  "Delete the current file and kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (cond
     ((not filename) (kill-buffer))
     ((vc-backend filename) (vc-delete-file filename))
     (:else
      (delete-file filename)
      (kill-buffer)))))

(provide 'stante-lib-file-commands)

;; Local Variables:
;; coding: utf-8
;; End:

;;; stante-lib-file-commands.el ends here
