;;; init.el --- Stante Pede: Instantly awesome Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012, 2013 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stante-pede.git
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

;; My personal Emacs configuration.

;;; Code:


;; Guard against Emacs 24
(when (version< emacs-version "24.3.50")
  (error "Stante Pede needs Emacs trunk, but this is %s!" emacs-version))


;;;; Package management

(require 'cask "~/.cask/cask" :no-error)
(cask-initialize)

(defconst stante-vendor-dir (locate-user-emacs-file "vendor")
  "Directory for embedded 3rd party extensions.")


;;;; Requires

(require 'dash)
(require 's)
(require 'f)

(require 'rx)


;;;; Package configuration and initialization

(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.

Forward compatibility wrapper."
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))

(defmacro stante-after (feature &rest forms)
  "After FEATURE is loaded, evaluate FORMS.

FORMS is byte compiled.

FEATURE may be a named feature or a file name, see
`eval-after-load' for details."
  (declare (indent 1) (debug t))
  `(progn
     (eval-when-compile
       ;; Require the feature during compilation to avoid compiler
       ;; errors/warnings. Since `eval-when-compile' also evaluated during macro
       ;; expansion we check whether the current file is really being compiled
       (when (bound-and-true-p byte-compile-current-file)
         ,(if (stringp feature)
              `(load ,feature :no-message :no-error)
            `(require ',feature nil :no-error))))
     ;; Register FORMS to be eval'ed after FEATURE
     (with-eval-after-load ',feature ,@forms)))

(defun stante-auto-modes (&rest modes-and-patterns)
  "Add MODES-AND-PATTERNS to `auto-mode-alist'.

MODES-AND-PATTERNS is of the form `(mode1 pattern1 pattern2 …
mode2 pattern3 pattern4)'.  For each major mode symbol, add auto
mode entries for all subsequent patterns until the next major
mode symbol."
  (--each (-partition-by-header #'symbolp modes-and-patterns)
    (pcase-let ((`(,mode . ,patterns) it))
      (--each patterns
        (add-to-list 'auto-mode-alist (cons it mode))))))

(defconst stante-font-lock-keywords
  `((,(rx "(" symbol-start
          (group (or "stante-after" "stante-auto-modes"))
          symbol-end
          (optional (one-or-more (syntax whitespace))
                    symbol-start
                    (group (one-or-more (or (syntax word) (syntax symbol))))
                    symbol-end))
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t)))
  "Our font lock keywords for Lisp modes.")

(stante-after lisp-mode
  (--each '(emacs-lisp-mode lisp-interaction-mode)
    (font-lock-add-keywords it stante-font-lock-keywords :append)))

(stante-after ielm
  (font-lock-add-keywords 'inferior-emacs-lisp-mode
                          stante-font-lock-keywords :append))


;;;; Environment fixup
(stante-after exec-path-from-shell
  (add-to-list 'exec-path-from-shell-variables "EMAIL"))

(when (and (not (eq system-type 'windows-nt)) (display-graphic-p))
  (exec-path-from-shell-initialize)

  (-when-let (email (getenv "EMAIL"))
    (setq user-mail-address email)))


;; The custom file
(defconst stante-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(stante-after cus-edit
  (setq custom-file stante-custom-file))

(load stante-custom-file :no-error :no-message)


;; OS X support

(stante-after ns-win
  (setq ns-pop-up-frames nil            ; Don't pop up new frames from the
                                        ; workspace
        mac-option-modifier 'meta       ; Option is simply the natural Meta
        mac-command-modifier 'meta      ; But command is a lot easier to hit
        mac-right-option-modifier 'none ; Keep right option for accented input
        ;; Just in case we ever need these keys
        mac-function-modifier 'hyper
        mac-right-command-modifier 'super))

;; Prefer GNU utilities over the BSD variants in Emacs, because the GNU tools
;; integrate better with Emacs
(defconst stante-gnu-ls (and (eq system-type 'darwin) (executable-find "gls"))
  "Path to GNU ls on OS X.")

(stante-after files
  (when stante-gnu-ls
    ;; Use GNU ls if available
    (setq insert-directory-program stante-gnu-ls)))

(stante-after dired
  (when (and (eq system-type 'darwin) (not stante-gnu-ls))
    ;; Don't probe for --dired flag in Dired, because we already know that GNU
    ;; ls is missing!
    (setq dired-use-ls-dired nil)))

(stante-after grep
  ;; Use GNU find on OS X, if possible
  (-when-let (gfind (and (eq system-type 'darwin) (executable-find "gfind")))
    (setq find-program gfind)))

(stante-after locate
  ;; Use mdfind as locate substitute on OS X, to utilize the Spotlight database
  (-when-let (mdfind (and (eq system-type 'darwin) (executable-find "mdfind")))
    (setq locate-command mdfind)))

;; Utility functions for OS X
(defun stante-id-of-bundle (bundle)
  "Get the ID of a BUNDLE.

BUNDLE is the user-visible name of the bundle as string.  Return
the id of the bundle as string.

These bundle IDs are normally constant.  Thus you may use this
function to determine the ID once, and then hard-code it in your
code."
  (let ((script (format "id of app \"%s\"" bundle)))
    (car (process-lines "osascript" "-e" script))))

(defun stante-path-of-bundle (id)
  "Get the path of a bundle with ID.

ID is the bundle ID (see `stante-id-of-bundle' as string.  Return
the directory path of the bundle as string."
  (let ((query (format "kMDItemCFBundleIdentifier == '%s'" id)))
    (car (process-lines "mdfind" query))))

(eval-and-compile
  (defun stante-homebrew-prefix (&optional formula)
    "Get the homebrew prefix for FORMULA.

Without FORMULA, get the homebrew prefix itself.

Return nil, if homebrew is not available, or if the prefix
directory does not exist."
    (let ((prefix (condition-case nil
                      (car (apply #'process-lines "brew" "--prefix"
                                  (when formula (list formula))))
                    (error nil))))
      (when (and prefix (file-directory-p prefix))
        prefix)))

  (defun stante-homebrew-installed-p (&optional formula)
    "Determine whether a homebrew FORMULA is installed.

Without FORMULA determine whether Homebrew itself is available."
    (if formula
        (when (stante-homebrew-prefix formula) t)
      (when (executable-find "brew") t))))


;;;; User interface

;; Get rid of tool bar and menu bar, except on OS X, where the menu bar is
;; present anyway, so disabling it is pointless
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(unless (eq system-type 'darwin)
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)))

;; No blinking and beeping, no startup screen and short Yes/No questions
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore
      inhibit-startup-screen t)
(fset 'yes-or-no-p 'y-or-n-p)

;; Choose Font and color theme.  We try to use Anonymous Pro from
;; http://www.marksimonson.com/fonts/view/anonymous-pro or Inconsolata (from the
;; Google Webfont directory).  On OS X, we need to give these fonts a larger
;; size.  If neither is available, we fall back to the standard faces of OS X
;; (Menlo), Linux (DejaVu Sans Mono) or Windows (Consolas, Courier New)
(defconst stante-preferred-monospace-fonts
  `(("Source Code Pro" . ,(if (eq system-type 'darwin) 130 110))
    ("Anonymous Pro" . ,(if (eq system-type 'darwin) 140 110))
    ("Anonymous Pro Minus" . ,(if (eq system-type 'darwin) 140 110))
    ("Inconsolata" . ,(if (eq system-type 'darwin) 140 110))
    ("Menlo" . 130)
    ("Consolas" . 130)
    ("DejaVu Sans Mono" 110)
    ("Courier New" . 130))
  "Preferred monospace fonts for Stante.

The `car' of each item is the font family, the `cdr' the preferred font size.")

(defconst stante-preferred-proportional-fonts
  '(("Lucida Grande" . 120)
    ("DejaVu Sans" . 110))
  "Preferred proportional fonts for Stante.

The `car' of each item is the font family, the `cdr' the preferred font size.")

(defun stante-first-existing-font (fonts)
  "Get the first existing font from FONTS."
  (--first (x-family-fonts (car it)) fonts))

(defun stante-choose-best-fonts ()
  "Choose the best fonts."
  (interactive)
  (-when-let (font  (stante-first-existing-font stante-preferred-monospace-fonts))
    (--each '(default fixed-pitch)
      (set-face-attribute it nil
                          :family (car font) :height (cdr font))))
  (-when-let (font (stante-first-existing-font stante-preferred-proportional-fonts))
    (set-face-attribute 'variable-pitch nil
                        :family (car font) :height (cdr font))))

(stante-choose-best-fonts)

(stante-after solarized
  ;; Disable variable pitch fonts in Solarized theme
  (setq solarized-use-variable-pitch nil))

;; (load-theme 'solarized-light :no-confirm)
;; (load-theme 'solarized-dark :no-confirm)
(load-theme 'zenburn :no-confirm)


;;;; The mode line

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Indicate position/total matches for incremental searches in the mode line
(stante-after anzu
  (diminish 'anzu-mode))

(global-anzu-mode)


;;;; The minibuffer

;; Save a minibuffer input history
(stante-after savehist
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval 180))
(savehist-mode t)

;; Boost file and buffer operations by flexible matching and the ability to
;; perform operations like deleting files or killing buffers directly from the
;; minibuffer
(stante-after ido
  (setq ido-enable-flex-matching t      ; Match characters if string doesn't
                                        ; match
        ido-create-new-buffer 'always   ; Create a new buffer if nothing matches
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t       ; Add virtual buffers for killed buffers
        ido-default-file-method 'selected-window
        ido-use-faces nil))             ; Prefer flx ido faces

(ido-mode t)
(ido-ubiquitous-mode)                   ; Use IDO everywhere…
(ido-at-point-mode)                     ; …even in `completion-at-point'
(flx-ido-mode)                          ; Powerful IDO flex matching

;; Configure Smex
(stante-after smex
  (setq smex-save-file (locate-user-emacs-file ".smex-items")))


;;;; Buffer, Windows and Frames

;; De-duplicate buffer names by prepending parts of the directory until the name
;; is unique, instead of just appending numbers.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*") ; Do not rename special buffers!

;; Clean stale buffers
(require 'midnight)

;; Move between windows with Shift + Arrow keys
(windmove-default-keybindings)

;; Undo and redo window configurations with C-c Left and C-c Right respectively
(winner-mode)

;; Prevent Ediff from spamming the frame
(stante-after ediff-wind
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; A utility command to quickly switch buffers, see
;; http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun stante-switch-to-previous-buffer ()
  "Switch to the previous buffer.

Repeated invocations toggle between the two most recently used
buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) :visible-ok)))

(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

;; Save buffers, windows and frames
(desktop-save-mode)


;;;; File handling

;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Store Tramp auto save files locally
(stante-after tramp
  (setq tramp-auto-save-directory (locate-user-emacs-file "tramp-auto-save")))

(stante-after dired
  ;; Power up dired
  (require 'dired-x)

  ;; Always revert Dired buffers on revisiting
  (setq dired-auto-revert-buffer t))

(stante-after dired-x
  (when (eq system-type 'darwin)
    ;; OS X bsdtar is mostly compatible with GNU Tar
    (setq dired-guess-shell-gnutar "tar")))

;; Update copyright when visiting files
(add-hook 'find-file-hook 'copyright-update)

;; Ignore uninteresting files
(ignoramus-setup)

;; Do not clobber user writeable files
(stante-after hardhat
  ;; Add local homebrew prefix to the list of protected directories.  Hardhat
  ;; itself only handles /usr/local/
  (when (eq system-type 'darwin)
    (-when-let (prefix (stante-homebrew-prefix))
      (add-to-list 'hardhat-fullpath-protected-regexps prefix))))
(global-hardhat-mode)

;; Save bookmarks immediately after a bookmark was added
(stante-after bookmark
  (setq bookmark-save-flag 1))

;; Track recent files
(stante-after recentf
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15))
(recentf-mode t)

;; Open recent files with IDO, see
;; http://emacsredux.com/blog/2013/04/05/recently-visited-files/
(stante-after recentf
  (defun stante-ido-find-recentf ()
    "Find a recent file with IDO."
    (interactive)
    (let ((file (ido-completing-read "Find recent file: " recentf-list nil t)))
      (when file
        (find-file file)))))

;; Save position in files
(require 'saveplace)
(setq-default save-place t)

;; View files read-only
(setq view-read-only t)

;; Automatically revert files on external changes (e.g. git checkout)
(global-auto-revert-mode 1)

;; Open files in external programs
(global-launch-mode)

(defun stante-launch-dired-dwim ()
  "Open the marked files externally.

If no files are marked, open the current directory instead."
  (let ((marked-files (dired-get-marked-files)))
    (if marked-files
        (launch-files marked-files :confirm)
      (launch-directory (dired-current-directory)))))

(defun stante-launch-dwim ()
  "Open the current file externally."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (stante-launch-dired-dwim)
    (if (buffer-file-name)
        (launch-file (buffer-file-name))
      (user-error "The current buffer is not visiting a file"))))

;; Utility commands for working with files, see:
;; http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
;; http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
;; https://github.com/bbatsov/prelude/blob/master/core/prelude-core.el
(defun stante-current-file ()
  "Gets the \"file\" of the current buffer.

The file is the buffer's file name, or the `default-directory' in
`dired-mode'."
  (if (eq major-mode 'dired-mode)
      default-directory
    (buffer-file-name)))

(defun stante-copy-filename-as-kill (&optional arg)
  "Copy the name of the currently visited file to kill ring.

With a zero prefix arg, copy the absolute file name.  With
\\[universal-argument], copy the file name relative to the
current buffer's `default-directory'.  Otherwise copy the
non-directory part only."
  (interactive "P")
  (-if-let* ((filename (stante-current-file))
             (name-to-copy (cond ((zerop (prefix-numeric-value arg)) filename)
                                 ((consp arg) (file-relative-name filename))
                                 (:else (file-name-nondirectory filename)))))
    (progn
      (kill-new name-to-copy)
      (message "%s" name-to-copy))
    (user-error "This buffer is not visiting a file")))

(defun stante-rename-file-and-buffer ()
  "Rename the current file and buffer."
  (interactive)
  (let* ((filename (buffer-file-name))
         (old-name (if filename
                       (file-name-nondirectory filename)
                     (buffer-name)))
         (new-name (read-file-name "New name: " nil nil nil old-name)))
    (cond
     ((not (and filename (file-exists-p filename))) (rename-buffer new-name))
     ((vc-backend filename) (vc-rename-file filename new-name))
     (:else
      (rename-file filename new-name :force-overwrite)
      (set-visited-file-name new-name :no-query :along-with-file)))))

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

;; Quickly edit init.el
(defun stante-find-user-init-file-other-window ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defvar stante-files-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'stante-ido-find-recentf)
    (define-key map (kbd "o") #'stante-launch-dwim)
    (define-key map (kbd "R") #'stante-rename-file-and-buffer)
    (define-key map (kbd "D") #'stante-delete-file-and-buffer)
    (define-key map (kbd "w") #'stante-copy-filename-as-kill)
    (define-key map (kbd "i") #'stante-find-user-init-file-other-window)
    map)
  "Keymap for file operations.")


;;;; Basic editing

;; Decent coding system
(prefer-coding-system 'utf-8)

;; Drag stuff around with Meta-Shift-Arrows
(stante-after drag-stuff
  (setq drag-stuff-modifier '(meta shift))

  ;; Drag Stuff is incompatible with Org, because it shadows many useful Org
  ;; bindings.  This doesn't do much harm, because Org has its own structural
  ;; movement commands
  (add-to-list 'drag-stuff-except-modes 'org-mode)

  (diminish 'drag-stuff-mode "⇅"))
(drag-stuff-global-mode)

;; Make `kill-whole-line' indentation aware
(defun stante-smart-kill-whole-line (&optional arg)
  "Kill whole line and move back to indentation.

Kill the whole line with function `kill-whole-line' and then move
`back-to-indentation'."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))

;; Some other utilities
(defun stante-smart-backward-kill-line ()
  "Kill line backwards and re-indent."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

(defun stante-smart-open-line ()
  "Insert empty line after the current line."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

;; Make C-a toggle between beginning of line and indentation
(defun stante-back-to-indentation-or-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; A missing autoload
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              tab-width 8)
;; Make Tab complete if the line is indented
(setq tab-always-indent 'complete)

;; Indicate empty lines at the end of a buffer in the fringe
(setq indicate-empty-lines t)

;; Highlight bad whitespace
(stante-after whitespace
  (diminish 'whitespace-mode "␣")
  ;; Highlight tabs, empty lines at beg/end, trailing whitespaces and overlong
  ;; portions of lines via faces.  Also indicate tabs via characters
  (setq whitespace-style '(face indentation space-after-tab space-before-tab
                                tab-mark empty trailing lines-tail)
        whitespace-line-column nil))    ; Use `fill-column' for overlong lines

;; Clean up whitespace
(stante-after whitespace-cleanup-mode
  ;; Always clean up whitespace
  (setq whitespace-cleanup-mode-only-if-initially-clean nil))

(--each '(prog-mode-hook text-mode-hook conf-mode-hook)
  (add-hook it #'whitespace-mode)
  (add-hook it #'whitespace-cleanup-mode))

;; A function to disable highlighting of long lines in modes
(stante-after whitespace
  (defun stante-whitespace-style-no-long-lines ()
    "Configure `whitespace-mode' for Org.

Disable the highlighting of overlong lines."
    (setq-local whitespace-style (-difference whitespace-style
                                              '(lines lines-tail)))
    (when whitespace-mode
      (whitespace-mode -1)
      (whitespace-mode 1))))

;; Delete the selection instead of inserting
(delete-selection-mode)

;; Save the contents of the clipboard to kill ring before killing
(setq save-interprogram-paste-before-kill t)

;; Configure a reasonable fill column, indicate it in the buffer and enable
;; automatic filling
(setq-default fill-column 80)
(--each '(prog-mode-hook text-mode-hook)
  (add-hook it 'fci-mode))
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(define-minor-mode stante-auto-fill-comments-mode
  "Minor mode to auto-fill comments only."
  :lighter nil
  :keymap nil
  (cond
   (stante-auto-fill-comments-mode
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode 1))
   (:else
    (kill-local-variable 'comment-auto-fill-only-comments)
    (auto-fill-mode -1))))
(add-hook 'prog-mode-hook 'stante-auto-fill-comments-mode)

;; Choose wrap prefix automatically
(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)

;; Configure scrolling
(setq scroll-margin 0                   ; Drag the point along while scrolling
      scroll-conservatively 1000        ; Never recenter the screen while scrolling
      scroll-error-top-bottom t         ; Move to beg/end of buffer before
                                        ; signalling an error
      ;; These settings make trackpad scrolling on OS X much more predictable
      ;; and smooth
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1))

;; Give us narrowing back!
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Same for region casing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Highlight the current line and editing operations in the buffer
(global-hl-line-mode 1)
(require 'volatile-highlights)          ; Doesn't autoload :|
(diminish 'volatile-highlights-mode)
(volatile-highlights-mode t)

;; Power up undo
(stante-after undo-tree (diminish 'undo-tree-mode "⤺"))
(global-undo-tree-mode)

;; Nicify page breaks
(stante-after page-break-lines (diminish 'page-break-lines-mode))
(global-page-break-lines-mode)

;; On the fly syntax checking
(stante-after flycheck
  (setq flycheck-completion-system 'grizzl))
(global-flycheck-mode)

;; An Emacs server for `emacsclient'
(require 'server)
(unless (server-running-p) (server-start))


;;;; Multiple cursors
(defvar stante-multiple-cursors-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'mc/edit-lines)
    (define-key map (kbd "C-a") #'mc/edit-beginnings-of-lines)
    (define-key map (kbd "C-e") #'mc/edit-ends-of-lines)
    (define-key map (kbd "C-s") #'mc/mark-all-in-region)
    (define-key map (kbd "n") #'mc/mark-next-like-this)
    (define-key map (kbd "p") #'mc/mark-previous-like-this)
    (define-key map (kbd "e") #'mc/mark-more-like-this-extended)
    (define-key map (kbd "h") #'mc/mark-all-like-this-dwim)
    map))


;;;; Smartparens
(require 'smartparens-config)           ; Setup standard configuration

(stante-after smartparens
  (setq sp-autoskip-closing-pair 'always
        ;; Don't kill the entire symbol on C-k
        sp-hybrid-kill-entire-symbol nil)

  ;; Smartparens bindings
  ;; Movement and navigation
  (define-key sp-keymap (kbd "C-M-f") #'sp-forward-sexp)
  (define-key sp-keymap (kbd "C-M-b") #'sp-backward-sexp)
  (define-key sp-keymap (kbd "C-M-u") #'sp-backward-up-sexp)
  (define-key sp-keymap (kbd "C-M-d") #'sp-down-sexp)
  (define-key sp-keymap (kbd "C-M-p") #'sp-backward-down-sexp)
  (define-key sp-keymap (kbd "C-M-n") #'sp-up-sexp)
  ;; Deleting and killing
  (define-key sp-keymap (kbd "C-M-k") #'sp-kill-sexp)
  (define-key sp-keymap (kbd "C-M-w") #'sp-copy-sexp)
  ;; Depth changing
  (define-key sp-keymap (kbd "M-s") #'sp-splice-sexp)
  (define-key sp-keymap (kbd "M-<up>") #'sp-splice-sexp-killing-backward)
  (define-key sp-keymap (kbd "M-<down>") #'sp-splice-sexp-killing-forward)
  (define-key sp-keymap (kbd "M-r") #'sp-splice-sexp-killing-around)
  (define-key sp-keymap (kbd "M-?") #'sp-convolute-sexp)
  ;; Barfage & Slurpage
  (define-key sp-keymap (kbd "C-)")  #'sp-forward-slurp-sexp)
  (define-key sp-keymap (kbd "C-<right>") #'sp-forward-slurp-sexp)
  (define-key sp-keymap (kbd "C-}")  #'sp-forward-barf-sexp)
  (define-key sp-keymap (kbd "C-<left>") #'sp-forward-barf-sexp)
  (define-key sp-keymap (kbd "C-(")  #'sp-backward-slurp-sexp)
  (define-key sp-keymap (kbd "C-M-<left>") #'sp-backward-slurp-sexp)
  (define-key sp-keymap (kbd "C-{")  #'sp-backward-barf-sexp)
  (define-key sp-keymap (kbd "C-M-<right>") #'sp-backward-barf-sexp)
  ;; Miscellaneous commands
  (define-key sp-keymap (kbd "M-S") #'sp-split-sexp)
  (define-key sp-keymap (kbd "M-J") #'sp-join-sexp)
  (define-key sp-keymap (kbd "C-M-t") #'sp-transpose-sexp))

(smartparens-global-mode)
(show-smartparens-global-mode)          ; Show parenthesis


;;;; Completion and expansion

;; Configured hippie-expand reasonably
(stante-after hippie-exp
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

;; In `completion-at-point', do not pop up silly completion buffers for less
;; than five candidates.  Cycle instead.
(setq completion-cycle-threshold 5)

;; Enable auto-completion
(stante-after company
  (diminish 'company-mode "•")

  ;; Make auto completion a little less aggressive.
  (setq company-idle-delay 1.0
        company-begin-commands '(self-insert-command)
        company-show-numbers t))        ; Easy navigation to candidates with
                                        ; M-<n>
(global-company-mode)


;;;; Spell checking

;; Warn if the spell checker is missing
(unless (executable-find "aspell")
  (message "Aspell not found.  Spell checking may not be available!"))

(stante-after ispell
  (setq ispell-dictionary "en"          ; Default dictionary
        ispell-silently-savep t))       ; Don't ask when saving the private dict

(stante-after flyspell
  ;; Free M-Tab and C-M-i, and never take it again!
  (define-key flyspell-mode-map "\M-\t" nil)
  (setq flyspell-use-meta-tab nil
        ;; Make Flyspell less chatty
        flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)

  (add-hook 'flyspell-mode-hook #'auto-dictionary-mode)

  (diminish 'flyspell-mode "✓"))

(--each '(text-mode-hook message-mode-hook)
  (add-hook it 'turn-on-flyspell))
(add-hook 'prog-mode-hook 'flyspell-prog-mode)


;;;; AUCTeX

(require 'tex-site nil :no-error)

;; Some standard defaults
(stante-after tex
  (setq TeX-parse-self t                ; Parse documents to provide completion
                                        ; for packages, etc.
        TeX-auto-save t                 ; Automatically save
        TeX-clean-confirm nil           ; Do not ask for confirmation when
                                        ; cleaning
        ;; Provide forward and inverse search with SyncTeX
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex)
  (setq-default TeX-master nil          ; Ask for the master file
                TeX-engine 'luatex      ; Use a modern engine
                TeX-PDF-mode t)         ; Create PDFs by default

  ;; Replace the rotten Lacheck with Chktex
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s"))

(stante-after latex
  (--each '(LaTeX-math-mode             ; Easy math input
            LaTeX-preview-setup         ; Setup LaTeX preview
            reftex-mode)                ; Cross references on steroids
    (add-hook 'LaTeX-mode-hook it))

  ;; Add support for latexmk
  (auctex-latexmk-setup))

;; Find Skim.app on OS X, for Sycntex support, which Preview.app lacks.
(defun stante-find-skim-bundle ()
    "Return the location of the Skim bundle, or nil if Skim is not installed.

Skim is an advanced PDF viewer for OS X with SyncTex support.
See http://skim-app.sourceforge.net/ for more information."
    (stante-path-of-bundle "net.sourceforge.skim-app.skim"))

  (defun stante-find-skim-displayline ()
    "Return the path of the displayline frontend of Skim.

Return nil if Skim is not installed.  See `stante-find-skim-bundle'."
    (-when-let (skim-bundle (stante-find-skim-bundle))
      (executable-find (expand-file-name "Contents/SharedSupport/displayline"
                                         skim-bundle))))

(stante-after tex
  (defun stante-TeX-find-view-programs-os-x ()
    "Find TeX view programs on OS X.

Populate `TeX-view-program-list' with installed viewers."
    ;; The default application, usually Preview
    (add-to-list 'TeX-view-program-list
                 '("Default application" "open %o"))
    ;; Skim if installed
    (-when-let (skim-displayline (stante-find-skim-displayline))
      (add-to-list 'TeX-view-program-list
                   `("Skim" (,skim-displayline " -b -r %n %o %b")))))

  (defun stante-TeX-select-view-programs-os-x ()
    "Select the best view programs on OS X.

Choose Skim if available, or fall back to the default application."
    ;; Find view programs
    (stante-TeX-find-view-programs-os-x)
    (setq TeX-view-program-selection
          `((output-dvi "Default application")
            (output-html "Default application")
            ;; Use Skim if installed for SyncTex support.
            (output-pdf ,(if (assoc "Skim" TeX-view-program-list)
                             "Skim" "Default application")))))

  (defun stante-TeX-select-view-programs ()
    "Select the best view programs for the current platform."
    (when (eq system-type 'darwin)
      (stante-TeX-select-view-programs-os-x)))

  ;; Select best viewing programs
  (stante-TeX-select-view-programs))

;; Configure BibTeX
(stante-after bibtex
  (bibtex-set-dialect 'biblatex)        ; Use a modern dialect
  ;; Exhaustive cleanup and reformatting of entries, to keep Bibtex files in
  ;; good shape
  (setq bibtex-entry-format '(opts-or-alts
                              required-fields
                              numerical-fields
                              whitespace
                              realign
                              last-comma
                              delimiters
                              unify-case
                              strings
                              sort-fields)))

;; Configure RefTeX
(stante-after reftex
  (setq reftex-plug-into-AUCTeX t       ; Plug into AUCTeX
        ;; Recommended optimizations
        reftex-enable-partial-scans t
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t)

  ;; Provide basic RefTeX support for biblatex
  (unless (assq 'biblatex reftex-cite-format-builtin)
    (add-to-list 'reftex-cite-format-builtin
                 '(biblatex "The biblatex package"
                            ((?\C-m . "\\cite[]{%l}")
                             (?t . "\\textcite{%l}")
                             (?a . "\\autocite[]{%l}")
                             (?p . "\\parencite{%l}")
                             (?f . "\\footcite[][]{%l}")
                             (?F . "\\fullcite[]{%l}")
                             (?x . "[]{%l}")
                             (?X . "{%l}"))))
    (setq reftex-cite-format 'biblatex)))

;; Plug reftex into bib-cite
(stante-after bib-cite
  (setq bib-cite-use-reftex-view-crossref t)) ; Plug into bibcite



;;;; ReStructuredText editing
(stante-after rst
  ;; Indent with 3 spaces after all kinds of literal blocks
  (setq rst-indent-literal-minimized 3
        rst-indent-literal-normal 3)

  ;; Free C-= for `expand-region'. `rst-adjust' is still on C-c C-= and C-c C-a
  ;; C-a
  (define-key rst-mode-map (kbd "C-=") nil)

  (sp-with-modes 'rst-mode
    (sp-local-pair "*" "*")
    (sp-local-pair "**" "**")
    (sp-local-pair "`" "`")
    (sp-local-pair "``" "``")))


;;;; Markdown editing

;; Why doesn't Markdown Mode do this itself?!
(stante-auto-modes 'markdown-mode (rx "." (or "md" "markdown") string-end))

;; Find a suitable processor
(stante-after markdown-mode
  (defconst stante-markdown-commands
    '(("kramdown")
      ("markdown2" "-x" "fenced-code-blocks")
      ("pandoc"))
    "Markdown processors we try to use.")

  (defun stante-find-markdown-processor ()
    "Find a suitable markdown processor.

Search for a suitable markdown processor using
`stante-markdown-commands' and set `markdown-command' properly.

Return the new `markdown-command' or signal an error if no
suitable processor was found."
    (interactive)
    ;; Clear previous command
    (setq markdown-command
          (mapconcat #'shell-quote-argument
                     (--first (executable-find (car it)) stante-markdown-commands)
                     " "))
    (unless markdown-command
      (error "No markdown processor found"))
    markdown-command)

  (stante-find-markdown-processor)

  (sp-with-modes '(markdown-mode gfm-mode)
    (sp-local-pair "*" "*" :bind "C-*")
    (sp-local-pair "`" "`")
    (sp-local-tag "s" "```scheme" "```")
    (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags)))

;; Don't do filling in GFM mode, where line breaks are significant, and do not
;; highlight overlong lines.  Instead enable visual lines.
(stante-after markdown-mode
  (--each '(turn-off-fci-mode turn-off-auto-fill visual-line-mode)
    (add-hook 'gfm-mode-hook it))

  (stante-after whitespace
    (add-hook 'gfm-mode-hook #'stante-whitespace-style-no-long-lines)))


;;;; HTML editing

(stante-after sgml-mode
  (require 'simplezen))


;;;; Various markup languages
(stante-after yaml-mode
  ;; YAML is kind of a mixture between text and programming language, and hence
  ;; derives from `fundamental-mode', so we enable a good mixture of our hooking
  ;; explicitly
  (--each '(whitespace-mode
            whitespace-cleanup-mode
            stante-auto-fill-comments-mode
            fci-mode
            flyspell-prog-mode)
    (add-hook 'yaml-mode-hook it)))


;;;; Symbol “awareness”

;; Navigate occurrences of the symbol under point with M-n and M-p
(add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)

;; Highlight the symbol under point
(stante-after highlight-symbol
  (setq highlight-symbol-idle-delay 0.4 ; Highlight almost immediately
        highlight-symbol-on-navigation-p t) ; Highlight immediately after
                                            ; navigation
  (diminish 'highlight-symbol-mode))
(add-hook 'prog-mode-hook #'highlight-symbol-mode)

(defvar stante-symbol-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'highlight-symbol-occur)
    (define-key map (kbd "%") #'highlight-symbol-query-replace)
    (define-key map (kbd "n") #'highlight-symbol-next-in-defun)
    (define-key map (kbd "p") #'highlight-symbol-prev-in-defun)
    map)
  "Keymap for symbol operations.")


;;;; Programming utilities

;; Regular expression helpers
(rxt-global-mode)                       ; Powerful explanation and conversion
                                        ; functions for regular expressions in
                                        ; the C-c / map

;; Font-lock enhancements
(cl-lib-highlight-initialize)                      ; Font lock for cl-lib
(cl-lib-highlight-warn-cl-initialize)              ; Warning face for deprecated
                                        ; cl functions
(add-hook 'prog-mode-hook #'number-font-lock-mode) ; Font lock for numeric
                                        ; literals


;;;; Basic Lisp editing

(defvar stante-lisp-common-modes
  '(rainbow-delimiters-mode)            ; Color parenthesis according to nesting
  "Common modes for Lisp editing.")

(stante-after rainbow-delimiters (diminish 'rainbow-delimiters-mode))

;; Improve Smartparens support for Lisp editing
(defvar stante-smartparens-lisp-mode-map
  (let ((map (make-sparse-keymap)))
    ;; More clever filling and new line insertion
    (define-key map (kbd "M-q") #'sp-indent-defun)
    (define-key map (kbd "C-j") #'sp-newline)
    map)
  "Keymap for `stante-smartparens-lisp-mode'.")

(define-minor-mode stante-smartparens-lisp-mode
  "A minor mode to enable Lisp editing with Smartparens.

When enabled, this mode essentially just adds some new key
bindings."
  :init-value nil)

(defun stante-smartparens-setup-lisp-modes (modes)
  "Setup Smartparens Lisp support in MODES.

Add Lisp pairs and tags to MODES, and use the a special, more strict
keymap `stante-smartparens-lisp-mode-map'."
  (when (symbolp modes)
    (setq modes (list modes)))
  ;; Wrap expressions with M-( (just like in Paredit)
  (sp-local-pair modes "(" nil :bind "M-(")
  (--each modes
    (let ((hook (intern (format "%s-hook" (symbol-name it)))))
      ;; Be strict about delimiters
      (add-hook hook #'smartparens-strict-mode)
      ;; Add our own keymap for some additional clever bindings
      (add-hook hook #'stante-smartparens-lisp-mode))))


;;;; Emacs Lisp

;; Teach Emacs about Emacs scripts and Cask/Carton files
(add-to-list 'interpreter-mode-alist '("emacs" . emacs-lisp-mode))
(stante-auto-modes 'emacs-lisp-mode (rx "/" (or "Cask" "Carton") string-end))

;; Enable some common Emacs Lisp helper modes
(defvar stante-emacs-lisp-common-modes
  (append
   '(turn-on-eldoc-mode                 ; Show function signatures in echo area
     elisp-slime-nav-mode)              ; Navigate to symbol definitions
   stante-lisp-common-modes)
  "Common modes for Emacs Lisp editing.")

(stante-after lisp-mode
  (--each stante-emacs-lisp-common-modes
    (add-hook 'emacs-lisp-mode-hook it)
    (add-hook 'lisp-interaction-mode-hook it))

  (defun stante-auto-compile-user-init-file ()
    "Enable `auto-compile-mode' for `user-init-file'."
    (when (and (buffer-file-name) (f-same? (buffer-file-name) user-init-file))
      (auto-compile-mode)))

  ;; Some more Emacs Lisp editing hooks
  (--each '(checkdoc-minor-mode         ; Check doc conventions when eval'ing
                                        ; expressions
            stante-auto-compile-user-init-file) ; Automatically compile init.el
    (add-hook 'emacs-lisp-mode-hook it))

  ;; Smartparens support for Emacs Lisp editing
  (stante-smartparens-setup-lisp-modes '(emacs-lisp-mode
                                         lisp-interaction-mode))

  ;; Load ERT to support unit test writing and running
  (require 'ert))

(stante-after flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(stante-after ielm
  (--each stante-emacs-lisp-common-modes
    (add-hook 'ielm-mode-hook it))

  ;; Smartparens support for IELM
  (stante-smartparens-setup-lisp-modes 'inferior-emacs-lisp-mode))

;; Indicate Auto Compile mode
(stante-after auto-compile (diminish 'auto-compile-mode "⏎"))

;; Now de-clutter the mode line
(stante-after eldoc (diminish 'eldoc-mode))
(stante-after checkdoc (diminish 'checkdoc-minor-mode))
(stante-after elisp-slime-nav (diminish 'elisp-slime-nav-mode "↪"))


;;;; Clojure

;; Enable some common Clojure helper modes
(defvar stante-clojure-common-modes
  (append '(subword-mode) stante-lisp-common-modes))

(stante-after clojure-mode
  ;; Standard Lisp/Clojure goodies for Clojure Mode
  (stante-smartparens-setup-lisp-modes 'clojure-mode)
  (--each stante-clojure-common-modes
    (add-hook 'clojure-mode-hook it))

  (add-hook 'clojure-mode-hook #'clojure-test-mode))

(stante-after cider-repl-mode
  ;; Standard Lisp/Clojure goodies for the Cider Repl
  (stante-smartparens-setup-lisp-modes 'cider-repl-mode)
  (--each stante-clojure-common-modes
    (add-hook 'cider-repl-mode-hook it)))

(stante-after cider-mode
  ;; Eldoc for Cider
  (add-hook 'cider-mode-hook #'cider-turn-on-eldoc-mode))

(stante-after nrepl-client
  ;; Hide Nrepl connection buffers from the buffer list
  (setq nrepl-hide-special-buffers t))


;;;; Python

(stante-after python
  (--each '(stante-python-filling subword-mode)
    (add-hook 'python-mode-hook it))

  ;; Fill according to PEP 8
  (defun stante-python-filling ()
    "Configure filling for Python."
    ;; PEP 8 recommends a maximum of 79 characters
    (setq fill-column 79))

  ;; Use a decent syntax and style checker
  (setq python-check-command "flake8"))


;;;; Ruby
(stante-after ruby-mode
  ;; Setup inf-ruby and Robe
  (--each '(robe-mode inf-ruby-minor-mode)
    (add-hook 'ruby-mode-hook it))

  ;; Easily switch to Inf Ruby from compilation modes to Inf Ruby
  (inf-ruby-switch-setup))

(stante-after robe
  (stante-after company
    (push 'company-robe company-backends)))


;;;; Shell scripting

;; Teach Emacs about Zsh scripts
(stante-auto-modes 'sh-mode (rx ".zsh" string-end))

;; Shell script indentation styles
(stante-after sh-script
  (setq sh-styles-alist
        '(("zsh"
           (sh-basic-offset . 2)
           (sh-first-lines-indent . 0)
           (sh-indent-after-case . +)
           (sh-indent-after-do . +)
           (sh-indent-after-done . 0)
           (sh-indent-after-else . +)
           (sh-indent-after-if . +)
           (sh-indent-after-loop-construct . +)
           (sh-indent-after-open . +)
           (sh-indent-comment)
           (sh-indent-for-case-alt . ++)
           (sh-indent-for-case-label . +)
           (sh-indent-for-continuation . +)
           (sh-indent-for-do . 0)
           (sh-indent-for-done . 0)
           (sh-indent-for-else . 0)
           (sh-indent-for-fi . 0)
           (sh-indent-for-then . 0))))

  (add-hook 'sh-mode-hook (apply-partially #'sh-load-style "zsh")))


;;;; Misc programming languages

;; Coffeescript: Indentation
(stante-after coffee-mode
  (setq coffee-tab-width 2))

;; Javascript: Indentation
(stante-after js2-mode
  (setq-default js2-basic-offset 2))

(stante-auto-modes 'js2-mode (rx "." (or "js" "json") string-end))

;; Haskell: Indentation, and some helpful modes
(stante-after haskell-mode
  (--each '(subword-mode
            turn-on-haskell-indentation
            turn-on-haskell-doc-mode
            turn-on-haskell-decl-scan)
    (add-hook 'haskell-mode-hook it)))

;; SCSS: Don't compile when saving (aka please don't spam my directories!)
(stante-after scss-mode
  (setq scss-compile-at-save nil))

;; XML: Complete closing tags, and insert XML declarations into empty files
(stante-after nxml-mode
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t))

;; PKGBUILD: Recognize PKGBUILD's
(stante-auto-modes 'pkgbuild-mode (rx "/PKGBUILD" string-end))

;; Feature Mode
(stante-after feature-mode
  ;; Add standard hooks for Feature Mode, since it is no derived mode
  (--each '(whitespace-mode whitespace-cleanup-mode flyspell-mode)
    (add-hook 'feature-mode it)))


;;;; General version control

(stante-after vc-hooks
  ;; Always follow symlinks to files in VCS repos
  (setq vc-follow-symlinks t))

;; Highlight VCS diffs
;; Highlight changes to the current file in the fringe
(global-diff-hl-mode)
;; Highlight changed files in the fringe of Dired
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
;; Fall back to the display margin, if the fringe is unavailable
(unless (window-system)
  (diff-hl-margin-mode))

(defun stante-update-all-diff-hl-buffers ()
  "Update diff highlighting in all affected buffers."
  (when (fboundp 'diff-hl-update)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (bound-and-true-p diff-hl-mode)
          (diff-hl-update))))))


;;;; Git support

;; The one and only Git frontend
(stante-after magit
  ;; Shut up, Magit!
  (setq magit-save-some-buffers 'dontask
        magit-stage-all-confirm nil
        magit-unstage-all-confirm nil
        ;; Except when you ask something useful…
        magit-set-upstream-on-push t)

  ;; Update Diff highlighting after Magit operations
  (add-hook 'magit-refresh-file-buffer-hook #'diff-hl-update))

(stante-after git-commit-mode
  ;; Update Diff highlighting after Git commits from Git commit mode
  (advice-add 'git-commit-commit :after
              (lambda (&rest _r) (stante-update-all-diff-hl-buffers))))

(stante-after gist
  (setq gist-view-gist t))              ; View Gists in browser after creation

(defvar stante-gist-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'gist-region-or-buffer)
    (define-key map (kbd "l") #'gist-list)
    map)
  "Keymap for Gist operations.")


;;;; Tools and utilities

;; Powerful search and narrowing framework
;; Set the prefix key before loading to prevent Helm from ever claiming "C-x c"
(defvar helm-command-prefix-key)
(setq helm-command-prefix-key nil)
(require 'helm-config)

;; Some custom helm bindings
(define-key helm-command-map (kbd "A") #'helm-apropos)
(define-key helm-command-map (kbd "g") #'helm-do-grep)
(define-key helm-command-map (kbd "o") #'helm-occur)
(define-key helm-command-map (kbd "p") #'helm-projectile)

;; Project interaction
(stante-after projectile
  (diminish 'projectile-mode)

  ;; Replace Ack with Ag in Projectile
  (define-key projectile-mode-map [remap projectile-ack] #'projectile-ag)

  (setq projectile-completion-system 'grizzl))
(projectile-global-mode)

;; Quickly switch to IELM
(defun stante-switch-to-ielm ()
  "Switch to an ielm window.

Create a new ielm process if required."
  (interactive)
  (pop-to-buffer (get-buffer-create "*ielm*"))
  (ielm))

(stante-after ag
  (setq ag-reuse-buffers t              ; Don't spam buffer list with ag buffers
        ag-highlight-search t           ; A little fanciness
        ;; Use Projectile to find the project root
        ag-project-root-function (lambda (d) (let ((default-directory d))
                                               (projectile-project-root)))))

(defvar stante-ag-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'ag-regexp)
    (define-key map (kbd "p") #'ag-project-regexp)
    map)
  "Keymap for Ack and a Half.")

;; Google from Emacs, under C-c /
(google-this-mode)
(stante-after google-this (diminish 'google-this-mode))


;;;; Org mode
;; Tell Org where our files are located.  We keep them in Dropbox for easy
;; synchronization.
(stante-after org
  (setq org-directory (expand-file-name "~/Dropbox/Org")
        org-agenda-files (list org-directory)
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-completion-use-ido t        ; Complete with IDO in Org
        org-yank-adjusted-subtrees t)   ; Adjust level when yanking entire trees

  (make-directory org-directory :with-parents)

  ;; Plug windmove into Org
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  ;; Disable long lines highlighting in Org.  Org changes the visual appearance
  ;; of buffer text (e.g. link collapsing), thus text may appear shorter than
  ;; the fill column while it is not.  The whitespace mode highlighting is very
  ;; irritating in such cases.
  (stante-after whitespace
    (add-hook 'org-mode-hook #'stante-whitespace-style-no-long-lines))

  ;; Teach Smartparens about Org Mode markup
  (--each '("*" "/" "=" "~")
    (sp-local-pair 'org-mode it it)))

(stante-after ox-latex
  ;; Teach Org LaTeX exporter about KOMA script
  (add-to-list 'org-latex-classes
               '("scrartcl"
                 "\\documentclass{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; Configure Org mobile target folder and inbox.  Again, we use Dropbox to get
;; synchronization for free.
(stante-after org-mobile
  (setq org-mobile-directory "~/Dropbox/Org/Mobile"
        org-mobile-inbox-for-pull
        (expand-file-name "from-mobile.org" org-directory))

  (make-directory org-mobile-directory :with-parents))


;;;; Calendar
(stante-after calendar
  ;; In Europe we start on Monday
  (setq calendar-week-start-day 1))

;; Calfw autoloads (which should really be in calfw itself…)
(autoload 'cfw:open-calendar-buffer "calfw")
(autoload 'cfw:org-create-source "calfw-org")
(autoload 'cfw:ical-create-source "calfw-ical")

(defconst stante-ical-urls-file
  (locate-user-emacs-file "stante-ical-urls")
  "Path to the file storing private ICal URLs.")

(defun stante-ical-sources ()
  "Get calfw sources for private ICal URLs."
  (when (f-exists? stante-ical-urls-file)
    (->> (f-read-text stante-ical-urls-file 'utf-8)
      s-lines
      (--map (pcase-let* ((`(,name ,color ,url) (s-split " " it)))
               (cfw:ical-create-source name url color))))))

(defun stante-personal-calendar ()
  "Show my personal calendar."
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (cons (cfw:org-create-source "DarkGreen")
         (stante-ical-sources))))


;;;; E-Mail

;; Settings for sending mail via GMail
(stante-after sendmail
  (setq send-mail-function 'smtpmail-send-it))

(stante-after message
  (setq message-send-mail-function 'smtpmail-send-it))

(stante-after smtpmail
  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls
        smtpmail-smtp-user user-mail-address))

;; Settings for reading mail via Gnus
(stante-after gnus
  (setq gnus-select-method
        '(nnimap "gmail"
                 (nnimap-address "imap.gmail.com")
                 (nnimap-server-port 993)
                 (nnimap-stream ssl))))


;;;; Key bindings

;; Improve standard bindings
(global-set-key [remap execute-extended-command] #'smex)
(global-set-key [remap list-buffers] #'ibuffer)
(global-set-key [remap kill-whole-line] #'stante-smart-kill-whole-line)
(global-set-key [remap move-beginning-of-line]
                #'stante-back-to-indentation-or-beginning-of-line)
(global-set-key [remap dabbrev-expand] #'hippie-expand)
(global-set-key [remap isearch-forward] #'isearch-forward-regexp)
(global-set-key [remap isearch-backward] #'isearch-backward-regexp)
;; Complement standard bindings (the comments indicate the related bindings)
(global-set-key (kbd "M-X") #'smex-major-mode-commands)                  ; M-x
(global-set-key (kbd "C-<backspace>") #'stante-smart-backward-kill-line) ; C-S-backspace
(global-set-key (kbd "C-S-j") #'stante-smart-open-line)                  ; C-j
(global-set-key (kbd "M-Z") #'zap-up-to-char)                            ; M-z
(global-set-key (kbd "C-h A") #'apropos)                                 ; C-h a
(global-set-key (kbd "C-x p") #'proced)                                  ; C-x p
;; Find definition sources fast
(global-set-key (kbd "C-h F") #'find-function) ; C-h f
(global-set-key (kbd "C-h V") #'find-variable) ; C-h v

;; Key bindings for extension packages
(global-set-key (kbd "C-=") #'er/expand-region)
(global-set-key (kbd "C-c SPC") #'ace-jump-mode)
(global-set-key (kbd "C-c C-SPC") #'ace-jump-mode)
(global-set-key (kbd "C-x SPC") #'ace-jump-mode-pop-mark)

;; User key bindings in the C-c space.
(global-set-key (kbd "C-c A") #'org-agenda)
(global-set-key (kbd "C-c a") stante-ag-map)
(global-set-key (kbd "C-c b") #'stante-switch-to-previous-buffer)
(global-set-key (kbd "C-c C") #'org-capture)
(global-set-key (kbd "C-c c") 'helm-command-prefix)
(global-set-key (kbd "C-c f") stante-files-map)
(global-set-key (kbd "C-c G") stante-gist-map)
(global-set-key (kbd "C-c g") #'magit-status)
(global-set-key (kbd "C-c i") #'imenu)
(global-set-key (kbd "C-c m") stante-multiple-cursors-map)
(global-set-key (kbd "C-c o") #'occur)
(global-set-key (kbd "C-c S") #'stante-personal-calendar) ; S for Schedule
(global-set-key (kbd "C-c s") stante-symbol-keymap)
(global-set-key (kbd "C-c y") #'browse-kill-ring)
(global-set-key (kbd "C-c z") #'stante-switch-to-ielm)

(stante-after lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c e") #'macrostep-expand))

(stante-after sgml-mode
  (define-key html-mode-map (kbd "C-c e") #'simplezen-expand)
  (define-key html-mode-map (kbd "TAB") #'simplezen-expand-or-indent-for-tab))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
