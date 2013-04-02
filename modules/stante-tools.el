;;; stante-tools.el --- Stante Pede Modules: Various tools -*- lexical-binding: t; -*-
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

;; Provide support for various external tools.

;; Ack
;; ---
;;
;; Integrate ack via `ack-and-a-half' from
;; https://github.com/jhelwig/ack-and-a-half.
;;

;; Key bindings
;; ------------
;;
;; C-c c compiles the current buffer with `compile'.
;;
;; C-c a a searches for all files.
;;
;; C-c a s searches for all files of the same type as the current buffer.

;;; Code:

(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(define-prefix-command 'ack-and-a-half-map)
(define-key ack-and-a-half-map (kbd "a") #'ack-and-a-half)
(define-key ack-and-a-half-map (kbd "s") #'ack-and-a-half-same)
(global-set-key (kbd "C-c a") 'ack-and-a-half-map)

(global-set-key (kbd "C-c c") #'compile)

(provide 'stante-tools)

;; Local Variables:
;; coding: utf-8
;; End:

;;; stante-tools.el ends here
