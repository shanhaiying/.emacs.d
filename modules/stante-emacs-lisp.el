;;; stante-emacs-lisp.el --- Stante Pede Modules: Emacs LISP support
;;
;; Copyright (c) 2012, 2013 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stante-pede.git
;; Keywords: convenience languages

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

;; Configure Emacs LISP editing.

;; Documentation
;; -------------
;;
;; Show information about a function or variable at point in the echo area (see
;; `eldoc-mode').
;;
;; Check documentation style automatically with `checkdoc-minor-mode'.

;; Balanced parenthesis
;; --------------------
;;
;; Keep parenthesis balanced while editing with `paredit-mode'.
;;
;; Read the reference table at http://mumble.net/~campbell/emacs/paredit.html
;; for a list of available commands.

;; Bytecode
;; --------
;;
;; Remove byte code file when Emacs LISP buffers are saved.

;; Keybindings
;; -----------
;;
;; C-c C-z switches to an interactive Emacs LISP shell (see
;; `stante-switch-to-ielm' and `ielm').

;;; Code:

(require 'stante-lib-autoloads)
(require 'stante-programming)

(package-need 'paredit)
(package-need 'rainbow-delimiters)
(package-need 'elisp-slime-nav)


(defun stante-emacs-lisp-clean-byte-code (&optional buffer)
  "Remove byte code file corresponding to the Emacs Lisp BUFFER.

BUFFER defaults to the current buffer."
  (when (eq major-mode 'emacs-lisp-mode)
    (let ((bytecode (concat  (buffer-file-name buffer) "c")))
      (when (file-exists-p bytecode)
        (delete-file bytecode)))))

(defun stante-emacs-lisp-clean-byte-code-on-save ()
    "Arrange for byte code to be cleaned on save."
    (add-hook 'after-save-hook 'stante-emacs-lisp-clean-byte-code nil t))

(defun stante-emacs-lisp-ac-setup ()
  "Configure auto-complete for Emacs lisp."
  (setq ac-sources (append '(ac-source-features
                             ac-source-functions
                             ac-source-variables
                             ac-source-symbols)
                           ac-sources)))

(defun stante-emacs-lisp-switch-to-ielm ()
  "Switch to an ielm window.

Create a new ielm process if required."
  (interactive)
  (pop-to-buffer (get-buffer-create "*ielm*"))
  (ielm))

(after 'lisp-mode
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-eldoc-mode)
    (add-hook hook 'paredit-mode)
    (add-hook hook 'rainbow-delimiters-mode))

  (add-hook 'emacs-lisp-mode-hook
            'stante-emacs-lisp-clean-byte-code-on-save)

  ;; Check documentation in Emacs LISP
  (add-hook 'emacs-lisp-mode-hook 'checkdoc-minor-mode)

  (define-key emacs-lisp-mode-map (kbd "C-c z")
    'stante-emacs-lisp-switch-to-ielm)

  (after 'auto-complete
    (add-hook 'emacs-lisp-mode-hook 'stante-emacs-lisp-ac-setup))
  )

(provide 'stante-emacs-lisp)

;;; stante-emacs-lisp.el ends here
