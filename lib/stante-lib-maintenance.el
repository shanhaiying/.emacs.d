;;; stante-lib-maintenance.el --- Stante Pede Library: Maintenance functions
;;
;; Copyright (c) 2012, 2013 Sebastian Wiesner
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

;; Stante Pede maintenance.

;; `stante-update-autoload-file' updates the autoload definitions of Stante
;; Pede.
;;
;; `stante-byte-recompile' byte-compiles all Stante Pede modules.
;;
;; `stante-report-issue' reports an issue to the Stante Pede issue tracker.

;; Load `stante-lib-autoloads' to use the functions of this library.

;;; Code:

(require 'autoload)
(require 'stante-lib-package)
(eval-when-compile
  (require 'gh))

;;;###autoload
(defconst stante-autoload-file (concat stante-lib-dir "stante-lib-autoloads.el")
  "Location of the autoload file for the Stante Pede Library.")

;;;###autoload
(defun stante-update-autoload-file ()
  "Update the autoload file of Stante Pede."
  (interactive)
  (let ((generated-autoload-file stante-autoload-file))
    (update-directory-autoloads stante-lib-dir)))

;;;###autoload
(defun stante-byte-recompile (&optional force)
  "Byte-compile all modules of Stante pede."
  (interactive "P")
  (byte-recompile-directory stante-lib-dir 0 force)
  (byte-recompile-directory stante-modules-dir 0 force)
  (let ((init-file (file-name-sans-extension stante-init-file) ))
    (byte-recompile-file (concat init-file ".el") force 0)))

(defvar stante-report-issue-title nil
  "The title of
 the issue to report with `stante-report-issue'.")

(defvar stante-report-issue-gh-api nil
  "The Github API object to use for `stante-report-issue'.")

(defun stante-report-issue-new (title body)
  "Create a new Stante Pede issue with given TITLE and BODY.

The issue is submitted to the Github issue tracker of Stante
Pede."
  ;; Load and install GH just in time
  (package-need 'gh)
  (require 'gh)
  (unless stante-report-issue-gh-api
    (setq stante-report-issue-gh-api (gh-issues-api "Stante Pede issues API")))
  (let ((issue (make-instance 'gh-issues-issue
                              :body body :title title :state "open")))
    (gh-issues-issue-new stante-report-issue-gh-api
                         "lunaryorn" "stante-pede"
                         issue))
  (stante-report-issue-cancel))

(defun stante-report-issue-submit ()
  "Submit the issue."
  (interactive)
  (when stante-report-issue-title
    (let ((title stante-report-issue-title)
          (body (buffer-substring-no-properties (point-min) (point-max))))
      (if (string= "" body)
          (if (y-or-n-p "The issue comment is empty. Submit anyway?")
            (stante-report-issue-new title body)
            (message "Not submitted, the comment is empty."))
        (stante-report-issue-new title body)))))

(defun stante-report-issue-cancel ()
  "Abort edits and erase the issue."
  (interactive)
  (when stante-report-issue-title
    (set-buffer-modified-p nil)
    (erase-buffer)
    (kill-buffer)))

;;;###autoload
(defun stante-report-issue (title)
  "Report an issue with TITLE to Stante Pede.

If called interactively, prompt for TITLE.

Pop up a buffer to edit the issue comment.  If `gfm-mode' is
available, use it in this buffer, otherwise fall back to
`text-mode'.

In this buffer, use C-c C-c s to submit the issue,
and C-c C-c k to cancel the process. "
  (interactive "sTitle of this issue: ")
  (let ((issue-buffer (get-buffer-create
                       (format "*Stante Pede issue: %s*" title))))
    (with-current-buffer issue-buffer
      ;; Enable gfm-mode if available, otherwise fall back to standard text-mode
      (if (fboundp 'gfm-mode)
          (gfm-mode)
        (message "Add (require 'stante-markdown) for Markdown support in the \
issue comment.")
        (text-mode))
      (set (make-local-variable 'stante-report-issue-title) title)
      (local-set-key (kbd "C-c C-c s") 'stante-report-issue-submit)
      (local-set-key (kbd "C-c C-c k") 'stante-report-issue-cancel)
      ;; Disable saving
      (local-set-key (kbd "C-x C-s")
                     (lambda ()
                       (interactive)
                       (message "Not saved. Use C-c C-c s to submit this issue \
or C-c C-c k to cancel."))))
    (pop-to-buffer issue-buffer)))

(provide 'stante-lib-maintenance)

;;; stante-lib-maintenance.el ends here
