;;; stante-chrome.el --- Stante Pede Modules: Google Chrome support
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

;; Provide support for Google Chrome.

;; Edit server
;; -----------
;;
;; Start an edit server to enable Emacs editing from Chrome using the Edit with
;; Emacs Extension.
;;
;; See https://github.com/stsquad/emacs_chrome and
;; https://chrome.google.com/webstore/detail/edit-with-emacs/ljobjlafonikaiipfkggjbhkghgicgoh

;;; Code:

(require 'edit-server)

(setq edit-server-url-major-mode-alist
      '(("stackoverflow\\.com/.*$" . markdown-mode)
        ("github\\.com/.*$" . gfm-mode)
        ("\\(?:www\\.\\)python-forum.de/.*$" . bbcode-mode)))

(edit-server-start)

(provide 'stante-chrome)

;;; stante-chrome.el ends here
