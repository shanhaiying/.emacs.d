;;; stante-project.el --- Stante Pede Modules: Project management -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2013 Sebastian Wiesner
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

;; Project management for Stante Pede

;; Projectile
;; ----------
;;
;; Provide the Projectile mode for project management (see
;; https://github.com/bbatsov/projectile)

;;; Code:

(eval-when-compile
  '(require 'projetile))

(projectile-global-mode)

(after 'projectile
  (diminish 'projectile-mode))

(provide 'stante-project)

;; Local Variables:
;; coding: utf-8
;; End:

;;; stante-project.el ends here
