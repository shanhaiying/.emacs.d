;;; stante-org.el --- Stante Pede Modules: Org mode configuration -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012, 2013 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stantepede.git
;; Keywords: extensions tools

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

;; Provide configuration for `org-mode'.

;; Keybindings
;; -----------
;;
;; C-c a shows the Org agenda with `org-agenda'.
;;
;; C-c c captures something in Org using `org-capture'.

;;; Code:

(require 'stante-text)

(after 'org
  ;; Make windmove work in org-mode
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  ;; Electric indentation in Org breaks headlines
  (add-hook 'org-mode-hook #'stante-editor-disable-electric-indentation)

  ;; Use IDO for switching between org buffers
  (setq org-completion-use-ido t
        org-outline-path-complete-in-steps nil)

  ;; Put the Org directory into the Dropbox
  (setq org-directory (expand-file-name "~/Dropbox/Org")
        org-agenda-files (list org-directory))

  ;; Create the directory for Org files
  (unless (file-directory-p org-directory)
    (make-directory org-directory)))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(provide 'stante-org)

;; Local Variables:
;; coding: utf-8
;; End:

;;; stante-org.el ends here
