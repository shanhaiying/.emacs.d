;;; stante-completion.el --- Stante Pede Modules: Completion support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012, 2013 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stante-pede.git
;; Keywords: convenience vc tools

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

;; Add advanced completion via Company Mode.

;; Keybindings
;; -----------
;;
;; M-Tab starts completion with Company.

;;; Code:

(eval-when-compile
  (require 'company))

(global-company-mode)

(after 'company
  (diminish 'company-mode)

  ;; Make completion a little less aggressive
  (setq company-idle-delay 1.0
        company-begin-commands '(self-insert-command)
        ;; Make completion a bit more fancy
        company-show-numbers t))

(global-set-key (kbd "M-<tab>") 'company-complete)

(provide 'stante-completion)

;; Local Variables:
;; coding: utf-8
;; End:

;;; stante-completion.el ends here
