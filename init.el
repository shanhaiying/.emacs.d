;;; init.el --- Emacs configuration of Sebastian Wiesner -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2014 Sebastian Wiesner <swiesner@lunaryorn.com>
;;
;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://gihub.com/lunaryorn/.emacs.d
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

;; Emacs configuration of Sebastian Wiesner, functional programmer and Flycheck
;; maintainer.

;;; Code:


;;; Initialization
(when (version< emacs-version "25")
  (error "This configuration needs Emacs trunk, but this is %s!" emacs-version))

;; And disable the site default settings
(setq inhibit-default-init t)

(defun lunaryorn-warn-about-outdated-build ()
  "Warn about outdated build."
  (let ((time-since-build (time-subtract (current-time) emacs-build-time)))
    (when (> (time-to-number-of-days time-since-build) 7)
      (lwarn 'emacs :warning "Your Emacs build is more than a week old!"))))

(run-with-idle-timer 0 nil #'lunaryorn-warn-about-outdated-build)

;; Debugging
(setq message-log-max 10000)


;;; Package management

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(defalias 'lunaryorn-after 'with-eval-after-load)


;;; Requires

(require 'subr-x)
(require 'rx)


;;; Environment fixup
(use-package exec-path-from-shell
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :init
  (progn
    (when (string-match-p "/zsh$" (getenv "SHELL"))
      ;; Use a non-interactive shell.  We use a login shell, even though we have
      ;; our paths setup in .zshenv.  However, OS X adds global settings to the
      ;; login profile.  Notably, this affects /usr/texbin from MacTeX
      (setq exec-path-from-shell-arguments '("-l")))

    (dolist (var '("EMAIL" "PYTHONPATH" "INFOPATH"))
      (add-to-list 'exec-path-from-shell-variables var))

    (exec-path-from-shell-initialize)

    (setq user-mail-address (getenv "EMAIL"))

    ;; Re-initialize the `Info-directory-list' from $INFOPATH.  Since package.el
    ;; already initializes info, we need to explicitly add the $INFOPATH
    ;; directories to `Info-directory-list'.
    (dolist (dir (parse-colon-path (getenv "INFOPATH")))
      (when dir
        (add-to-list 'Info-directory-list dir)))))


;;; Customization interface
(defconst lunaryorn-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit
  :defer t
  :config (setq custom-file lunaryorn-custom-file))

(load lunaryorn-custom-file 'no-error 'no-message)


;;; OS X support
(use-package ns-win
  :defer t
  :if (eq system-type 'darwin)
  :config
  (setq ns-pop-up-frames nil            ; Don't pop up new frames from the
                                        ; workspace
        mac-option-modifier 'meta       ; Option is simply the natural Meta
        mac-command-modifier 'meta      ; But command is a lot easier to hit
        mac-right-command-modifier 'left
        mac-right-option-modifier 'none ; Keep right option for accented input
        ;; Just in case we ever need these keys
        mac-function-modifier 'hyper))

(use-package lunaryorn-osx
  :if (eq system-type 'darwin)
  :load-path "lisp/"
  :defines (lunaryorn-darwin-trash-tool)
  :init
  (if (executable-find lunaryorn-darwin-trash-tool)
      (defalias 'system-move-file-to-trash 'lunaryorn-darwin-move-file-to-trash)
    (warn "Trash support not available!
Install Trash from https://github.com/ali-rantakari/trash!
Homebrew: brew install trash")))


;;; User interface

;; Get rid of tool bar, menu bar and scroll bars.  On OS X we preserve the menu
;; bar, since the top menu bar is always visible anyway, and we'd just empty it
;; which is rather pointless.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No blinking and beeping, no startup screen, no scratch message and short
;; Yes/No questions.
(blink-cursor-mode -1)
(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      initial-scratch-message nil)
(fset 'yes-or-no-p #'y-or-n-p)
;; Opt out from the startup message in the echo area by simply disabling this
;; ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message #'ignore)

;;; Font setup
(let ((font "Source Code Pro")
      (size (pcase system-type
              (`darwin 13)
              (_ 10))))
  (if (x-family-fonts font)
      (set-frame-font (format "%s-%s" font size) nil t)
    (lwarn 'emacs :warning "%S font is missing!" font)))

(use-package unicode-fonts
  :ensure t
  :init (unicode-fonts-setup))

(use-package solarized
  :ensure solarized-theme
  :defer t
  :init (load-theme 'solarized-light 'no-confirm)
  :config t
  ;; Disable variable pitch fonts in Solarized theme
  (setq solarized-use-variable-pitch nil
        ;; Don't add too much colours to the fringe
        solarized-emphasize-indicators nil
        ;; I find different font sizes irritating.
        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0))


;;; The mode line

(use-package diminish
  :ensure t
  :defer t)

;; Standard stuff
(line-number-mode)
(column-number-mode)

(use-package fancy-battery
  :ensure t
  :init (fancy-battery-mode))

;; Indicate position/total matches for incremental searches in the mode line
(use-package anzu
  :ensure t
  :init (global-anzu-mode)
  ;; Please don't mess with my mode line
  :config (setq anzu-cons-mode-line-p nil)
  :diminish anzu-mode)

;; Show the current function name in the mode line
(use-package which-func
  :init (which-function-mode)
  :config
  (setq which-func-unknown "⊥" ; The default is really boring…
        which-func-format
        `((:propertize (" ➤ " which-func-current)
                       local-map ,which-func-keymap
                       face which-func
                       mouse-face mode-line-highlight
                       help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end"))))

;; Improve our mode line
(defvar lunaryorn-vc-mode-line
  '(" " (:propertize
         ;; Strip the backend name from the VC status information
         (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
                  (substring vc-mode (+ (length backend) 2))))
         face font-lock-variable-name-face))
  "Mode line format for VC Mode.")
(put 'lunaryorn-vc-mode-line 'risky-local-variable t)

(setq-default header-line-format
              '(which-func-mode ("" which-func-format " "))
              mode-line-format
              '("%e" mode-line-front-space
                ;; Standard info about the current buffer
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                ;; Some specific information about the current buffer:  Indicate
                ;; the presence of structured editing, with Paredit or SHM
                (paredit-mode (:propertize " ()" face bold))
                (structured-haskell-mode (:propertize shm-lighter face bold))
                (structured-haskell-repl-mode (:propertize shm-lighter
                                                           face bold))
                ;; Warn if whitespace isn't highlighted or cleaned in this
                ;; buffer.
                (:eval (unless buffer-read-only
                         (cond
                          ((not (bound-and-true-p whitespace-mode))
                           (propertize " SPACE" 'face '(bold error)))
                          ((not (bound-and-true-p whitespace-cleanup-mode))
                           (propertize " WSC" 'face 'warning)))))
                (projectile-mode projectile-mode-line)
                (vc-mode lunaryorn-vc-mode-line)   ; VC information
                (flycheck-mode flycheck-mode-line) ; Flycheck status
                (anzu-mode (:eval                  ; isearch pos/matches
                            (when (> anzu--total-matched 0)
                              (concat " " (anzu--update-mode-line)))))
                (multiple-cursors-mode mc/mode-line) ; Number of cursors
                ;; And the modes, which we don't really care for anyway
                " " mode-line-misc-info mode-line-modes mode-line-end-spaces)
              mode-line-remote
              '(:eval
                (when-let (host (file-remote-p default-directory 'host))
                  (propertize (concat "@" host) 'face
                              '(italic warning))))
              ;; Remove which func from the mode line, since we have it in the
              ;; header line
              mode-line-misc-info
              (assq-delete-all 'which-func-mode mode-line-misc-info))


;;; The minibuffer

;; Increase Emacs' memory of the past
(setq history-length 1000)

;; Save a minibuffer input history
(use-package savehist
  :init (savehist-mode t)
  :config (setq savehist-save-minibuffer-history t
                savehist-autosave-interval 180))

(use-package ido
  :init (progn
          (ido-mode)
          (ido-everywhere))
  :config
  (setq ido-enable-flex-matching t      ; Match characters if string doesn't
                                        ; match
        ido-create-new-buffer 'always   ; Create a new buffer if nothing matches
        ido-use-filename-at-point 'guess
        ;; Visit buffers and files in the selected window
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        ido-use-faces nil))             ; Prefer flx ido faces

(use-package ido-ubiquitous
  :ensure t
  :init (ido-ubiquitous-mode))

(use-package flx-ido
  :ensure t
  :init (flx-ido-mode))

(use-package ido-vertical-mode
  :ensure t
  :init (ido-vertical-mode))

;; Configure Smex
(use-package smex
  :ensure t
  :bind (([remap execute-extended-command] . smex)
         ("M-X" . smex-major-mode-commands)))

;; Tune `eval-expression'
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)


;;; Buffer, Windows and Frames

;;; Kill `suspend-frame'
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)

(use-package lunaryorn-buffers
  :load-path "lisp/"
  :commands (lunaryorn-force-save-some-buffers
             lunaryorn-do-not-kill-important-buffers)
  :init
  (progn
    (add-hook 'kill-buffer-query-functions
              #'lunaryorn-do-not-kill-important-buffers)

    ;; Autosave buffers when focus is lost, see
    ;; http://emacsredux.com/blog/2014/03/22/a-peek-at-emacs-24-dot-4-focus-hooks/
    (add-hook 'focus-out-hook #'lunaryorn-force-save-some-buffers)))

;; Make uniquify rename buffers like in path name notation
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

;; Clean stale buffers
(use-package midnight)

(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer))
  ;; Show VC Status in ibuffer
  :config (setq ibuffer-formats
                '((mark modified read-only vc-status-mini " "
                        (name 18 18 :left :elide)
                        " "
                        (size 9 -1 :right)
                        " "
                        (mode 16 16 :left :elide)
                        " "
                        (vc-status 16 16 :left)
                        " "
                        filename-and-process)
                  (mark modified read-only " "
                        (name 18 18 :left :elide)
                        " "
                        (size 9 -1 :right)
                        " "
                        (mode 16 16 :left :elide)
                        " " filename-and-process)
                  (mark " "
                        (name 16 -1)
                        " " filename))))

(use-package ibuffer-vc
  :ensure t
  :defer t
  ;; Group ibuffer by VC status
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(use-package windmove
  ;; Move between windows with Shift + Arrow keys
  :init (windmove-default-keybindings))

(use-package winner
  ;; Undo and redo window configurations with C-c Left and C-c Right
  ;; respectively
  :init (winner-mode))

;; Prevent Ediff from spamming the frame
(use-package ediff-wind
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

(use-package desktop
  ;; Save buffers, windows and frames
  :init (desktop-save-mode)
  :config (progn
            ;; Don't autosave desktops, it's too expensive.  Desktops aren't
            ;; that precious, and Emacs will save the desktop on exit anyway.
            (setq desktop-auto-save-timeout nil)
            (add-to-list 'desktop-modes-not-to-save 'magit-mode)))


;;; File handling

;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Delete files to trash
(setq delete-by-moving-to-trash
      (or (not (eq system-type 'darwin)) ; Trash is well supported on other
                                        ; systems
          (fboundp 'system-move-file-to-trash)))

;; Store Tramp auto save files locally
(use-package tramp
  :defer t
  :config
  (setq tramp-auto-save-directory (locate-user-emacs-file "tramp-auto-save")))

(use-package dired
  :defer t
  :config
  (progn
    ;; Power up dired
    (require 'dired-x)

    ;; Always revert Dired buffers on revisiting
    (setq dired-auto-revert-buffer t
          dired-listing-switches "-alh" ; Human-readable sizes by default
          )))

(use-package dired-x
  :defer t
  :config
  (when (eq system-type 'darwin)
    ;; OS X bsdtar is mostly compatible with GNU Tar
    (setq dired-guess-shell-gnutar "tar")))

;; Update copyright when visiting files
(add-hook 'find-file-hook #'copyright-update)

;; Ignore uninteresting files
(use-package ignoramus
  :ensure t
  :defer t
  :idle (ignoramus-setup)
  :idle-priority 10)

(use-package hardhat
  :ensure t
  :defer t
  :idle (global-hardhat-mode)
  :idle-priority 10)

;; Save bookmarks immediately after a bookmark was added
(use-package bookmark
  :defer t
  :bind (("C-c l b" . list-bookmarks))
  :config (setq bookmark-save-flag 1))

;; Track recent files
(use-package recentf
  :defer t
  :idle (recentf-mode)
  :idle-priority 10
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                              "/elpa/.*\\'" ; Package files
                              "/itsalltext/" ; It's all text temp files
                              ;; And all other kinds of boring files
                              #'ignoramus-boring-p)))

;; Save position in files
(use-package saveplace
  :config (setq-default save-place t))

;; View files read-only
(setq view-read-only t)

;; Automatically revert files on external changes (e.g. git checkout)
(global-auto-revert-mode 1)

;; Open files in external programs
(use-package launch
  :ensure t
  :idle (global-launch-mode)
  :idle-priority 10)

(use-package lunaryorn-files
  :load-path "lisp/"
  :bind (("C-c f D" . lunaryorn-delete-file-and-buffer)
         ("C-c f i" . lunaryorn-find-user-init-file-other-window)
         ("C-c f o" . lunaryorn-launch-dwim)
         ("C-c f r" . lunaryorn-ido-find-recentf)
         ("C-c f R" . lunaryorn-rename-file-and-buffer)
         ("C-c f w" . lunaryorn-copy-filename-as-kill)))

;;; Additional bindings for built-ins
(bind-key "C-c f L" #'add-dir-local-variable)
(bind-key "C-c f l" #'add-file-local-variable)
(bind-key "C-c f p" #'add-file-local-variable-prop-line)


;;; Basic editing
;; Make `kill-whole-line' indentation aware

;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              tab-width 8)
;; Make Tab complete if the line is indented
(setq tab-always-indent 'complete)

;; Electric pairing and code layout
(electric-pair-mode)
(electric-layout-mode)

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(setq indicate-empty-lines t
      require-final-newline t)

;; Delete the selection instead of inserting
(delete-selection-mode)

(setq kill-ring-max 200                 ; More killed items
      ;; Save the contents of the clipboard to kill ring before killing
      save-interprogram-paste-before-kill t)

;; Configure a reasonable fill column, indicate it in the buffer and enable
;; automatic filling
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)

(use-package lunaryorn-simple
  :bind (([remap kill-whole-line]        . lunaryorn-smart-kill-whole-line)
         ([remap move-beginning-of-line] . lunaryorn-back-to-indentation-or-beginning-of-line)
         ("C-<backspace>"                . lunaryorn-smart-backward-kill-line)
         ("C-S-j"                        . lunaryorn-smart-open-line))
  :commands (lunaryorn-auto-fill-comments-mode)
  ;; Auto-fill comments in programming modes
  :init (add-hook 'prog-mode-hook #'lunaryorn-auto-fill-comments-mode))

(use-package misc
  :bind (("M-Z" . zap-up-to-char)))

(use-package whitespace-cleanup-mode
  :ensure t
  :bind (("C-c t W" . whitespace-cleanup-mode))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  :diminish whitespace-cleanup-mode)

;; Subword/superword editing
(use-package subword
  :defer t
  :diminish subword-mode)

;; Choose wrap prefix automatically
(use-package adaptive-wrap
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode))

(use-package visual-regexp
  :ensure t
  :bind (("C-c r" . vr/query-replace)
         ("C-c R" . vr/replace)))

(use-package browse-kill-ring
  :ensure t
  :bind (("C-c y" . browse-kill-ring)))

(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp]      . easy-mark)))

(use-package align
  :bind (("C-c A a" . align)
         ("C-c A c" . align-current)
         ("C-c A r" . align-regexp)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m e"   . mc/mark-more-like-this-extended)
         ("C-c m h"   . mc/mark-all-like-this-dwim)
         ("C-c m l"   . mc/edit-lines)
         ("C-c m n"   . mc/mark-next-like-this)
         ("C-c m p"   . mc/mark-previous-like-this)
         ("C-c m r"   . vr/mc-mark)
         ("C-c m C-a" . mc/edit-beginnings-of-lines)
         ("C-c m C-e" . mc/edit-ends-of-lines)
         ("C-c m C-s" . mc/mark-all-in-region))
  :config
  (setq mc/mode-line
        ;; Simplify the MC mode line indicator
        '(:propertize (:eval (concat " " (number-to-string (mc/num-cursors))))
                      face font-lock-warning-face)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;; Power up undo
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode)

;; Line numbers
(use-package nlinum
  :ensure t
  :bind (("C-c t l" . nlinum-mode)))

;; Give us narrowing back!
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Same for region casing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; An Emacs server for `emacsclient'
(use-package server
  :defer t
  :idle (unless (server-running-p) (server-start)))

;; Additional keybindings
(bind-key [remap just-one-space] #'cycle-spacing)
(bind-key "M-Z" #'zap-up-to-char)


;;; Navigation and scrolling
(setq scroll-margin 0                   ; Drag the point along while scrolling
      scroll-conservatively 1000        ; Never recenter the screen while scrolling
      scroll-error-top-bottom t         ; Move to beg/end of buffer before
                                        ; signalling an error
      ;; These settings make trackpad scrolling on OS X much more predictable
      ;; and smooth
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1))

;; Jump to characters in buffers
(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-c j"   . ace-jump-mode)
         ("C-c J"   . ace-jump-mode-pop-mark))
  :config
  ;; Sync marks with Emacs built-in commands
  (ace-jump-mode-enable-mark-sync))

;; Nicify page breaks
(use-package page-break-lines
  :ensure t
  :init (global-page-break-lines-mode)
  :defer page-break-lines-modes)

;; Outline commands
(use-package outline
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'outline-minor-mode))
  :diminish outline-minor-mode)

(use-package imenu-anywhere
  :bind (("C-c i" . imenu-anywhere)))


;;; Highlights
(use-package whitespace                 ; Bad whitespace
  :bind (("C-c t w" . whitespace-mode))
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config
  ;; Highlight tabs, empty lines at beg/end, trailing whitespaces and overlong
  ;; portions of lines via faces.  Also indicate tabs via characters
  (setq whitespace-style '(face indentation space-after-tab space-before-tab
                                tab-mark empty trailing lines-tail)
        whitespace-line-column nil)     ; Use `fill-column' for overlong lines
  :diminish whitespace-mode)

;; A function to disable highlighting of long lines in modes
(defun lunaryorn-whitespace-style-no-long-lines ()
  "Configure `whitespace-mode' for Org.

Disable the highlighting of overlong lines."
  (setq-local whitespace-style (-difference whitespace-style
                                            '(lines lines-tail)))
  (when whitespace-mode
    (whitespace-mode -1)
    (whitespace-mode 1)))

(use-package hl-line                    ; Current line
  :init (global-hl-line-mode 1))

(use-package volatile-highlights        ; Buffer operations
  :ensure t
  :init (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)

(use-package paren                      ; Paired delimiters
  :init (show-paren-mode)
  :config (setq show-paren-when-point-inside-paren t
                show-paren-when-point-in-periphery t))

(use-package rainbow-delimiters         ; Delimiters by depth
  :ensure t
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'rainbow-delimiters-mode)))

(use-package hi-lock                    ; Custom regexp highlights
  :init (global-hi-lock-mode))


;;; Completion and expansion

;; In `completion-at-point', do not pop up silly completion buffers for less
;; than five candidates.  Cycle instead.
(setq completion-cycle-threshold 5)

;; Configure hippie-expand reasonably
(use-package hippie-exp
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

;; Enable auto-completion
(use-package company
  :ensure t
  :defer t
  :idle (global-company-mode)
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t))
  :diminish company-mode)

(use-package company-math
  :ensure t
  :defer t
  :init
  ;; Add backend for math characters
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-math-symbols-unicode)
    (add-to-list 'company-backends 'company-math-symbols-latex)))


;;; Spelling and syntax checking
(use-package ispell
  :defer t
  :config
  (progn
    (setq ispell-program-name (if (eq system-type 'darwin)
                                  (executable-find "aspell")
                                (executable-find "hunspell"))
          ispell-dictionary "en_GB"     ; Default dictionnary
          ispell-silently-savep t       ; Don't ask when saving the private dict
          ;; Increase the height of the choices window to take our header line
          ;; into account.
          ispell-choices-win-default-height 5)

    (unless ispell-program-name
      (warn "No spell checker available.  Install Hunspell or ASpell for OS X."))))

(use-package flyspell
  :bind (("C-c t s" . flyspell-mode))
  :init
  (progn
    (dolist (hook '(text-mode-hook message-mode-hook))
      (add-hook hook 'turn-on-flyspell))
    (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
  (progn
    (setq flyspell-use-meta-tab nil
          ;; Make Flyspell less chatty
          flyspell-issue-welcome-flag nil
          flyspell-issue-message-flag nil)

    ;; Free C-M-i for completion
    (define-key flyspell-mode-map "\M-\t" nil))
  :diminish flyspell-mode)

;; On the fly syntax checking
(use-package flycheck
  :ensure t
  :bind (("C-c l e" . list-flycheck-errors)
         ("C-c t f" . flycheck-mode))
  :idle (global-flycheck-mode)
  :config
  (progn
    (setq flycheck-completion-system 'ido)

    ;; Use italic face for checker name
    (set-face-attribute 'flycheck-error-list-checker-name nil :inherit 'italic))
  :diminish flycheck-mode)

(use-package flycheck-pos-tip
  :ensure t
  :defer t
  :init
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(use-package lunaryorn-flycheck
  :defer t
  :commands (lunaryorn-discard-undesired-html-tidy-error
             lunaryorn-flycheck-mode-line-status)
  :init (with-eval-after-load 'flycheck
          ;; Don't highlight undesired errors from html tidy
          (add-hook 'flycheck-process-error-functions
                    #'lunaryorn-discard-undesired-html-tidy-error)

          (setq flycheck-mode-line
                '(:eval (lunaryorn-flycheck-mode-line-status)))))


;;; LaTeX with AUCTeX

(use-package tex-site
  :ensure auctex)

;; TeX editing
(use-package tex
  :ensure auctex
  :defer t
  :config
  (progn
    (setq TeX-parse-self t              ; Parse documents to provide completion
                                        ; for packages, etc.
          TeX-auto-save t               ; Automatically save style information
          TeX-electric-sub-and-superscript t ; Automatically insert braces after
                                        ; sub- and superscripts in math mode
          ;; Don't insert magic quotes right away.
          TeX-quote-after-quote t
          ;; Don't ask for confirmation when cleaning
          TeX-clean-confirm nil
          ;; Provide forward and inverse search with SyncTeX
          TeX-source-correlate-mode t
          TeX-source-correlate-method 'synctex)
    (setq-default TeX-master nil        ; Ask for the master file
                  TeX-engine 'luatex    ; Use a modern engine
                  TeX-PDF-mode t)

    ;; Move to chktex
    (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s")))

(use-package tex-buf
  :ensure auctex
  :defer t
  ;; Don't ask for confirmation when saving before processing
  :config (setq TeX-save-query nil))

(use-package tex-style
  :ensure auctex
  :defer t
  :config
  ;; Enable support for csquotes
  (setq LaTeX-csquotes-close-quote "}"
        LaTeX-csquotes-open-quote "\\enquote{"))

(use-package tex-fold
  :ensure auctex
  :defer t
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package tex-mode
  :ensure auctex
  :defer t
  :config
  (font-lock-add-keywords 'latex-mode
                          `((,(rx "\\"
                                  symbol-start
                                  "fx" (1+ (or (syntax word) (syntax symbol)))
                                  symbol-end)
                             . font-lock-warning-face))))

(use-package latex
  :ensure auctex
  :defer t
  :config
  (progn
    ;; Teach TeX folding about KOMA script sections
    (setq TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
                              (,(rx (0+ space) "\\subsection*{") 3)
                              (,(rx (0+ space) "\\subsubsection*{") 4)
                              (,(rx (0+ space) "\\minisec{") 5))
          ;; No language-specific hyphens please
          LaTeX-babel-hyphen nil)

    (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)))    ; Easy math input

(use-package auctex-latexmk
  :ensure t
  :defer t
  :init (with-eval-after-load 'latex
          (auctex-latexmk-setup)))

;; find Skim.app on OS X, for Sycntex support, which Preview.app lacks.
(defun lunaryorn-find-skim-bundle ()
    "Return the location of the Skim bundle, or nil if Skim is not installed.

Skim is an advanced PDF viewer for OS X with SyncTex support.
See http://skim-app.sourceforge.net/ for more information."
    (lunaryorn-path-of-bundle "net.sourceforge.skim-app.skim"))

(defun lunaryorn-find-skim-displayline ()
  "Return the path of the displayline frontend of Skim.

Return nil if Skim is not installed.  See `lunaryorn-find-skim-bundle'."
  (when-let (skim-bundle (lunaryorn-find-skim-bundle))
    (executable-find (expand-file-name "Contents/SharedSupport/displayline"
                                       skim-bundle))))

(lunaryorn-after 'tex
  (defun lunaryorn-TeX-find-view-programs-os-x ()
    "Find TeX view programs on OS X.

Populate `TeX-view-program-list' with installed viewers."
    ;; The default application, usually Preview
    (add-to-list 'TeX-view-program-list
                 '("Default application" "open %o"))
    ;; Skim if installed
    (when-let (skim-displayline (lunaryorn-find-skim-displayline))
      (add-to-list 'TeX-view-program-list
                   `("Skim" (,skim-displayline " -b -r %n %o %b")))))

  (defun lunaryorn-TeX-select-view-programs-os-x ()
    "Select the best view programs on OS X.

Choose Skim if available, or fall back to the default application."
    ;; Find view programs
    (lunaryorn-TeX-find-view-programs-os-x)
    (setq TeX-view-program-selection
          `((output-dvi "Default application")
            (output-html "Default application")
            ;; Use Skim if installed for SyncTex support.
            (output-pdf ,(if (assoc "Skim" TeX-view-program-list)
                             "Skim" "Default application")))))

  (defun lunaryorn-TeX-select-view-programs ()
    "Select the best view programs for the current platform."
    (when (eq system-type 'darwin)
      (lunaryorn-TeX-select-view-programs-os-x)))

  ;; Select best viewing programs
  (lunaryorn-TeX-select-view-programs))

(use-package bibtex
  :defer t
  :config
  (progn
    ;; Run prog mode hooks for bibtex
    (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

    ;; Use a modern BibTeX dialect
    (bibtex-set-dialect 'biblatex)))

(defun lunaryorn-reftex-find-ams-environment-caption (environment)
  "Find the caption of an AMS ENVIRONMENT."
  (let ((re (rx-to-string `(and "\\begin{" ,environment "}"))))
    ;; Go to the beginning of the label first
    (re-search-backward re)
    (goto-char (match-end 0)))
  (if (not (looking-at (rx (zero-or-more space) "[")))
      (error "Environment %s has no title" environment)
    (let ((beg (match-end 0)))
      ;; Move point onto the title start bracket and move over to the end,
      ;; skipping any other brackets in between, and eventually extract the text
      ;; between the brackets
      (goto-char (1- beg))
      (forward-list)
      (buffer-substring-no-properties beg (1- (point))))))

;; Configure RefTeX
(use-package reftex
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config
  (progn
    ;; Plug into AUCTeX
    (setq reftex-plug-into-AUCTeX t
          ;; Automatically derive labels, and prompt for confirmation
          reftex-insert-label-flags '(t t)
          reftex-label-alist
          '(
            ;; Additional label definitions for RefTeX.
            ("definition" ?d "def:" "~\\ref{%s}"
             lunaryorn-reftex-find-ams-environment-caption
             ("definition" "def.") -3)
            ("theorem" ?h "thm:" "~\\ref{%s}"
             lunaryorn-reftex-find-ams-environment-caption
             ("theorem" "th.") -3)
            ("example" ?x "ex:" "~\\ref{%s}"
             lunaryorn-reftex-find-ams-environment-caption
             ("example" "ex") -3)
            ;; Algorithms package
            ("algorithm" ?a "alg:" "~\\ref{%s}"
             "\\\\caption[[{]" ("algorithm" "alg") -3)))

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
  :diminish reftex-mode)

;; Plug reftex into bib-cite
(use-package bib-cite
  :defer t
  :config (setq bib-cite-use-reftex-view-crossref t))


;;; ReStructuredText editing
(use-package rst
  :defer t
  :config
  ;; Indent with 3 spaces after all kinds of literal blocks
  (setq rst-indent-literal-minimized 3
        rst-indent-literal-normal 3)

  (bind-key "C-=" nil rst-mode-map)
  ;; For similarity with AUCTeX
  (bind-key  "C-c C-j" #'rst-insert-list rst-mode-map))


;;; Markdown editing
(use-package markdown-mode
  :ensure t
  ;; Use GFM Mode for Markdown files from It's All Text.  It's better for most
  ;; sites to not add hard line breaks to content.
  :mode ("/itsalltext/.*\\.md\\'" . gfm-mode)
  :config
  (progn
    ;; Use Pandoc to process Markdown
    (setq markdown-command "pandoc -s -f markdown -t html5")

    ;; No filling in GFM, because line breaks are significant.
    (add-hook 'gfm-mode-hook #'turn-off-auto-fill)
    ;; Use visual lines instead
    (add-hook 'gfm-mode-hook #'visual-line-mode)

    (bind-key "C-c C-s C" #'markdown-insert-gfm-code-block markdown-mode-map)
    (bind-key "C-c C-s P" #'markdown-insert-gfm-code-block markdown-mode-map)

    ;; Fight my habit of constantly pressing M-q.  We should not fill in GFM Mode.
    (bind-key "M-q" #'ignore gfm-mode-map)))


;;; YAML
(use-package yaml-mode
  :ensure t
  :defer t
  :config
  (progn
    ;; Whitespace handling and filling
    (add-hook 'yaml-mode-hook #'whitespace-mode)
    (add-hook 'yaml-mode-hook #'whitespace-cleanup-mode)
    (add-hook 'yaml-mode-hook #'lunaryorn-auto-fill-comments-mode)
    (add-hook 'yaml-mode-hook #'flyspell-prog-mode)))

(use-package ansible-doc
  :ensure t
  :defer t
  :init (add-hook 'yaml-mode-hook #'ansible-doc-mode)
  :diminish ansible-doc-mode)


;;; Graphviz
(use-package graphviz-dot-mode
  :ensure t
  :defer t
  :config
  (setq graphviz-dot-indent-width 4))


;;; Symbol “awareness”
(defun highlight-symbol-ag ()
  "Call `ag-project-regexp' with the symbol at point.

Needs ag.el from URL `https://github.com/Wilfred/ag.el'."
  (interactive)
  (unless (fboundp 'ag-project)
    (error "Please install ag.el from https://github.com/Wilfred/ag.el"))
  (if (thing-at-point 'symbol)
      (let ((highlight-symbol-border-pattern '("\\b" . "\\b")))
        (ag-project-regexp (highlight-symbol-get-symbol)))
    (error "No symbol at point")))

(use-package highlight-symbol
  :ensure t
  :defer t
  :bind
  (("C-c s %" . highlight-symbol-query-replace)
   ("C-c s a" . highlight-symbol-ag)
   ("C-c s n" . highlight-symbol-next-in-defun)
   ("C-c s o" . highlight-symbol-occur)
   ("C-c s p" . highlight-symbol-prev-in-defun))
  :init
  (progn
    ;; Navigate occurrences of the symbol under point with M-n and M-p
    (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
    ;; Highlight symbol occurrences
    (add-hook 'prog-mode-hook #'highlight-symbol-mode))
  :config
  (setq highlight-symbol-idle-delay 0.4     ; Highlight almost immediately
        highlight-symbol-on-navigation-p t) ; Highlight immediately after
                                        ; navigation
  :diminish highlight-symbol-mode)


;;; Programming utilities

;; Compilation from Emacs
(defun lunaryorn-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer.

Taken from http://stackoverflow.com/a/3072831/355252."
  (interactive)
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(use-package compile
  :bind (("C-c c" . compile)
         ("C-c C" . recompile))
  :config
  (progn
    (setq compilation-ask-about-save nil ; Just save before compiling
          compilation-always-kill t     ; Just kill old compile processes before
                                        ; starting the new one
          compilation-scroll-output 'first-error ; Automatically scroll to first
                                        ; error
          )

    ;; Colorize output of Compilation Mode, see
    ;; http://stackoverflow.com/a/3072831/355252
    (require 'ansi-color)
    (add-hook 'compilation-filter-hook #'lunaryorn-colorize-compilation-buffer)))

;; Font lock for numeric literals
(use-package highlight-numbers
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; Font lock for color values
(use-package rainbow-mode
  :ensure t
  :bind (("C-c t r" . rainbow-mode)))


;;; Generic Lisp
(use-package paredit
  :ensure t
  :defer t
  :init
  (dolist (hook '(eval-expression-minibuffer-setup-hook
                  emacs-lisp-mode-hook
                  inferior-emacs-lisp-mode-hook
                  clojure-mode-hook))
    (add-hook hook #'paredit-mode))
  :diminish paredit-mode)


;;; Emacs Lisp
(use-package elisp-slime-nav
  :ensure t
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  :diminish elisp-slime-nav-mode)

(use-package flycheck-cask
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(use-package flycheck-package
  :ensure t
  :defer t
  :init (with-eval-after-load 'flycheck (flycheck-package-setup)))

(use-package pcre2el
  :ensure t
  :init (rxt-global-mode))

(use-package macrostep
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'lisp-mode
    (bind-key "C-c e e" #'macrostep-expand emacs-lisp-mode-map)
    (bind-key "C-c e e" #'macrostep-expand lisp-interaction-mode-map)))

(use-package ielm
  :bind (("C-c u z" . ielm)))

(use-package lisp-mode
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :mode ("/Cask\\'" . emacs-lisp-mode)
  :config
  (progn
    (require 'ert)))

;; Utility functions
(defun lunaryorn-find-cask-file (other-window)
    "Find the Cask file for this buffer.

When OTHER-WINDOW is non-nil, find the Cask file in another
window."
    (interactive "P")
    (unless (buffer-file-name)
      (user-error "The buffer has no file"))
    (let ((directory (locate-dominating-file (buffer-file-name) "Cask")))
      (unless directory
        (user-error "No Cask file found for this file"))
      (funcall (if other-window #'find-file-other-window #'find-file)
               (expand-file-name "Cask" directory))))

(defun lunaryorn-emacs-lisp-current-feature ()
  "Return the feature provided by the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp (rx line-start "(provide '"))
      (symbol-name (symbol-at-point)))))

;; Hippie expand for Emacs Lisp
(lunaryorn-after 'hippie-exp
  (defun lunaryorn-try-complete-lisp-symbol-without-namespace (old)
    "Hippie expand \"try\" function which expands \"-foo\" to \"modname-foo\" in elisp."
    (unless old
      (he-init-string (he-lisp-symbol-beg) (point))
      (when (string-prefix-p "-" he-search-string)
        (let ((mod-name (lunaryorn-emacs-lisp-current-feature)))
          (when mod-name
            (setq he-expand-list (list (concat mod-name he-search-string)))))))

    (when he-expand-list
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list nil)
      t))

  (defun lunaryorn-emacs-lisp-setup-hippie-expand ()
    (setq-local hippie-expand-try-functions-list
                (append hippie-expand-try-functions-list
                        '(lunaryorn-try-complete-lisp-symbol-without-namespace))))

  (lunaryorn-after 'lisp-mode
    (dolist (hook '(emacs-lisp-mode-hook lisp-interaction-mode-hook))
      (add-hook hook #'lunaryorn-emacs-lisp-setup-hippie-expand)))

  (lunaryorn-after 'ielm
    (add-hook 'ielm-mode-hook #'lunaryorn-emacs-lisp-setup-hippie-expand)))

(bind-key "C-c t d" #'toggle-debug-on-error)


;;; Clojure

(use-package clojure-mode
  :ensure cider
  :defer t
  :config
  (progn
    (add-hook 'clojure-mode-hook #'paredit-mode)
    (add-hook 'clojure-mode-hook #'cider-mode)))

;; Extra font-locking for Clojure
(use-package clojure-mode-extra-font-locking
  :ensure clojure-mode-extra-font-locking
  :defer t
  :init (with-eval-after-load 'clojure-mode
          (require 'clojure-mode-extra-font-locking)))

(use-package nrepl-client
  :ensure cider
  :defer t
  :config (setq nrepl-hide-special-buffers t))

(use-package cider-repl
  :ensure cider
  :defer t
  ;; Increase the history size and make it permanent
  (setq cider-repl-history-size 1000
        cider-repl-history-file (locate-user-emacs-file "cider-repl-history")))


;;; Python
(use-package python
  :defer t
  :config
  (progn
    ;; PEP 8 compliant filling rules, 79 chars maximum
    (add-hook 'python-mode-hook
              (lambda () (setq fill-column 79)))
    (add-hook 'python-mode-hook #'subword-mode)
    ;; Lookup, navigation and completion
    (add-hook 'python-mode-hook #'anaconda-mode)
    ;; Pick Flycheck executables from current virtualenv automatically
    (add-hook 'python-mode-hook #'lunaryorn-flycheck-setup-python)

    ;; Use a decent syntax and style checker
    (setq python-check-command "pylint"
          ;; Use IPython as interpreter
          python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i"))

  (lunaryorn-after 'flycheck
    (defun lunaryorn-flycheck-setup-python-executables ()
      "Setup Python executables based on the current virtualenv."
      (let ((exec-path (python-shell-calculate-exec-path)))
        (setq-local flycheck-python-pylint-executable
                    (executable-find "pylint"))
        (setq-local flycheck-python-flake8-executable
                    (executable-find "flake8"))))

    (defun lunaryorn-flycheck-setup-python ()
      "Setup Flycheck in Python buffers."
      (add-hook 'hack-local-variables-hook
                #'lunaryorn-flycheck-setup-python-executables 'local))))

(use-package company-anaconda
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-anaconda)))

(use-package pip-requirements
  :ensure t
  :defer t)


;;; Ruby
(use-package inf-ruby
  :ensure t
  :defer t
  :init
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
  :config
  ;; Easily switch to Inf Ruby from compilation modes to Inf Ruby
  (inf-ruby-switch-setup))

(use-package robe
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-robe)))


;;; Rust
(use-package rust-mode
  :ensure t
  :defer t)

(use-package toml-mode
  :ensure t
  :defer t)

(use-package flycheck-rust
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


;;; Haskell

;; This Haskell setup needs:
;;
;; cabal install hasktags haskell-docs hoogle haskell-stylish
;;
;; Additionally, to be installed from source:
;;
;; - https://github.com/chrisdone/ghci-ng
;; - https://github.com/chrisdone/structured-haskell-mode

(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'haskell-mode-hook #'subword-mode)           ; Subword navigation
    (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode) ; Scan and navigate
                                        ; declarations
    ;; Insert module templates into new buffers
    (add-hook 'haskell-mode-hook #'haskell-auto-insert-module-template)

    ;; Automatically run hasktags
    (setq haskell-tags-on-save t
          ;; Suggest adding/removing imports as by GHC warnings and Hoggle/GHCI
          ;; loaded modules respectively
          haskell-process-suggest-remove-import-lines t
          haskell-process-auto-import-loaded-modules t
          haskell-process-use-presentation-mode t ; Don't clutter the echo area
          haskell-process-show-debug-tips nil     ; Disable tips
          haskell-process-log t                   ; Log debugging information
          ;; Suggest imports automatically with Hayoo.  Hayoo is slower because
          ;; it's networked, but it covers all of hackage, which is really an
          ;; advantage.
          haskell-process-suggest-hoogle-imports nil
          haskell-process-suggest-hayoo-imports t
          ;; Use GHCI NG from https://github.com/chrisdone/ghci-ng
          haskell-process-path-ghci "ghci-ng")

    (add-to-list 'haskell-process-args-cabal-repl "--with-ghc=ghci-ng")

    (bind-key "C-c e d" #'haskell-describe haskell-mode-map)
    (bind-key "C-c e h" #'haskell-hayoo haskell-mode-map)
    (bind-key "C-c e H" #'haskell-hoogle haskell-mode-map)
    (bind-key "C-c e i" #'haskell-navigate-imports haskell-mode-map)
    (bind-key "C-c f c" #'haskell-cabal-visit-file haskell-mode-map)

    (bind-key "C-c C-t" #'haskell-mode-show-type-at
              interactive-haskell-mode-map)
    (bind-key "M-." #'haskell-mode-goto-loc
              interactive-haskell-mode-map)
    (bind-key "C-c e u" #'haskell-mode-find-uses
              interactive-haskell-mode-map)))

(use-package haskell-interactive-mode
  :ensure haskell-mode
  :defer t
  :init (dolist (hook '(haskell-mode-hook haskell-cabal-mode-hook))
          (add-hook hook #'interactive-haskell-mode))
  :config (add-hook 'haskell-interactive-mode-hook #'subword-mode))

(use-package shm
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'haskell-mode-hook #'structured-haskell-mode)
    (add-hook 'haskell-interactive-mode-hook #'structured-haskell-repl-mode))
  :config (progn
            (require 'shm-case-split)
            (bind-key "C-c e s" #'shm/case-split))
  :diminish structured-haskell-mode)

(use-package flycheck-haskell
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))


;;; OCaml
(defun lunaryorn-opam-init ()
  "Initialize OPAM in this Emacs."
  (with-temp-buffer
    (when-let (opam (executable-find "opam"))
      (let ((exit-code (call-process "opam" nil t nil "config" "env" "--sexp")))
        (if (not (equal exit-code 0))
            (warn "opam config env failed with exit code %S and output:
%s" exit-code (buffer-substring-no-properties (point-min) (point-max)))
          (goto-char (point-min))
          (let ((sexps (read (current-buffer))))
            (skip-chars-forward "[:space:]")
            (unless (eobp)
              (warn "Trailing text in opam config env:\n%S"
                    (buffer-substring-no-properties (point) (point-max))))
            (pcase-dolist (`(,var ,value) sexps)
              (setenv var value)))))))
  ;; Now update `exec-path' and `load-path'
  (setq exec-path (append (parse-colon-path (getenv "PATH"))
                          (list exec-directory))))

(lunaryorn-opam-init)

(use-package tuareg
  :ensure t
  :defer t
  :config
  (progn
    ;; Disable SMIE indentation in Tuareg.  It's just broken currently…
    (setq tuareg-use-smie nil)

    ;; Please, Tuareg, don't kill my imenu
    (define-key tuareg-mode-map [?\C-c ?i] nil)))

;; Advanced completion engine for OCaml
(use-package merlin
  :ensure t
  :defer t
  :init
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  :config
  ;; Use Merlin from current OPAM env
  (setq merlin-command 'opam))

(use-package flycheck-ocaml
  :ensure t
  :defer t
  :init (with-eval-after-load 'flycheck
          (flycheck-ocaml-setup))
  :config (with-eval-after-load 'merlin
            ;; Disable Merlin's own error checking in favour of Flycheck
            (setq merlin-error-after-save nil)))


;;; Shell scripting
(use-package sh-script
  :mode ("\\.zsh\\'" . sh-mode)
  :config
  ;; Use two spaces in shell scripts.
  (setq sh-indentation 2                ; The basic indentation
        sh-basic-offset 2               ; The offset for nested indentation
        ))


;;; Misc programming languages
(use-package puppet-mode
  :ensure t
  :defer t
  :config
  ;; Fontify variables in Puppet comments
  (setq puppet-fontify-variables-in-comments t))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\(?:on\\)\\'"            ; For Javascript and KSON
  :config (setq-default js2-basic-offset 2))

(use-package nxml-mode
  :defer t
  ;; Complete closing tags, and insert XML declarations into empty files
  :config (setq nxml-slash-auto-complete-flag t
                nxml-auto-insert-xml-declaration-flag t))

(use-package feature-mode
  :ensure t
  :defer t
  :config
  (progn
    ;; Add standard hooks for Feature Mode, since it is no derived mode
    (add-hook 'feature-mode-hook #'whitespace-mode)
    (add-hook 'feature-mode-hook #'whitespace-cleanup-mode)
    (add-hook 'feature-mode-hook #'flyspell-mode)))

(use-package cmake-mode
  :ensure t
  :defer t)


;;; Proof General
(use-package proof-site
  :load-path "vendor/ProofGeneral/generic"
  :config
  (setq proof-three-window-enable nil   ; More predictable window management
        ;; Automatically process the script up to point when inserting a
        ;; terminator.  Really handy in Coq.
        proof-electric-terminator-enable t))

;; Proof General has a rather strange way of creating this variable
(defvar coq-one-command-per-line)
(setq coq-one-command-per-line nil)

(use-package proof-script
  :defer t
  :config
  (add-hook 'proof-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

(use-package isar
  :defer t
  :config
  ;; Don't highlight overlong lines in Isar, since Unicode Tokens conceal the
  ;; true line length
  (add-hook 'isar-mode-hook #'lunaryorn-whitespace-style-no-long-lines 'append))


;;; Special modes
(auto-image-file-mode)                  ; Visit images as images


;;; Documentation
(use-package info
  :defer t
  :config
  ;; Fix the stupid `Info-quoted' face.  Courier is an abysmal face, so go back
  ;; to the default face.
  (set-face-attribute 'Info-quoted nil :family 'unspecified
                      :inherit font-lock-constant-face))


;;; General version control
(use-package vc-hooks
  :defer t
  ;; Always follow symlinks to files in VCS repos
  (setq vc-follow-symlinks t))

(use-package diff-hl
  :ensure t
  :defer t
  :init
  (progn
    ;; Highlight changes to the current file in the fringe
    (global-diff-hl-mode)
    ;; Highlight changed files in the fringe of Dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

    ;; Fall back to the display margin, if the fringe is unavailable
    (unless (display-graphic-p)
      (diff-hl-margin-mode))))


;;; Git support
(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status))
  :config
  (progn
    ;; Shut up, Magit!
    (setq magit-save-some-buffers 'dontask
          magit-stage-all-confirm nil
          magit-unstage-all-confirm nil
          ;; Except when you ask something useful…
          magit-set-upstream-on-push t
          ;; Use IDO for completion
          magit-completing-read-function #'magit-ido-completing-read)

    ;; Auto-revert files after Magit operations
    (magit-auto-revert-mode))
  :diminish magit-auto-revert-mode)

;; Git modes
(use-package git-commit-mode            ; Git commit message mode
  :ensure t
  :defer t)
(use-package gitconfig-mode             ; Git configuration mode
  :ensure t
  :defer t)
(use-package gitignore-mode             ; .gitignore mode
  :ensure t
  :defer t)
(use-package gitattributes-mode         ; Git attributes mode
  :ensure t
  :defer t)
(use-package git-rebase-mode            ; Mode for git rebase -i
  :ensure t
  :defer t)

;; Go back in Git time
(use-package git-timemachine
  :ensure t
  :defer t)


;;; Tools and utilities
(use-package locate
  :defer t
  :config
  ;; Use mdfind as locate substitute on OS X, to utilize the Spotlight database
  (when-let (mdfind (and (eq system-type 'darwin) (executable-find "mdfind")))
    (setq locate-command mdfind)))

;; Powerful file and code search
(use-package ag
  :ensure t
  :bind (("C-c a a" . ag-regexp)
         ("C-c a A" . ag)
         ("C-c a d" . ag-dired-regexp)
         ("C-c a D" . ag-dired)
         ("C-c a f" . ag-files)
         ("C-c a k" . ag-kill-other-buffers)
         ("C-c a K" . ag-kill-buffers))
  :config
  (setq ag-reuse-buffers t            ; Don't spam buffer list with ag buffers
        ag-highlight-search t         ; A little fanciness
        ;; Use Projectile to find the project root
        ag-project-root-function (lambda (d) (let ((default-directory d))
                                               (projectile-project-root)))))

(use-package wgrep
  :ensure t
  :defer t)

(use-package wgrep-ag
  :ensure t
  :defer t)

;; Project management
(use-package projectile
  :ensure t
  :defer t
  :init (projectile-global-mode)
  :config
  (progn
    (setq projectile-completion-system 'ido
          projectile-find-dir-includes-top-level t
          projectile-mode-line '(:propertize
                                 (:eval (concat " " (projectile-project-name)))
                                 face font-lock-constant-face))

    ;; Replace Ack with Ag in Projectile commander
    (def-projectile-commander-method ?a
      "Find ag on project."
      (call-interactively 'projectile-ag))

    ;; Use ag to search in Projectile projects
    (bind-key "s a" #'ag-project-regexp projectile-command-map)
    (bind-key "s A" #'ag-project projectile-command-map)
    (bind-key "s d" #'ag-project-dired-regexp projectile-command-map)
    (bind-key "s D" #'ag-project-dired projectile-command-map)
    (bind-key "s f" #'ag-project-files projectile-command-map)
    ;; For symmetry with `lunaryorn-ag-map'
    (bind-key "s k" #'ag-kill-other-buffers projectile-command-map)
    (bind-key "s K" #'ag-kill-buffers projectile-command-map))
  :diminish projectile-mode)

;; Insert date and time
(defun lunaryorn-insert-current-date (iso)
  "Insert the current date at point.

When ISO is non-nil, insert the date in ISO 8601 format.
Otherwise insert the date as Mar 04, 2014."
  (interactive "P")
  (insert (format-time-string (if iso "%F" "%b %d, %Y"))))

;; Bug references
(use-package bug-reference
  :defer t
  :init (progn (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
               (add-hook 'text-mode-hook #'bug-reference-mode)))

(use-package paradox
  :ensure t
  :bind (("C-c l p" . paradox-list-packages))
  :config
  ;; Don't ask for a token, please
  (setq paradox-github-token t))

(bind-key "C-c l P" #'package-list-packages-no-fetch)
(bind-key "C-c o" #'occur)
(bind-key "C-x p" #'proced)


;;; Calendar
(use-package calendar
  :defer t
  :config
  ;; In Europe we start on Monday
  (setq calendar-week-start-day 1))


;;; Net & Web

;; Web browsing
(use-package browse-url
  :defer t
  :config (setq browse-url-browser-function #'eww-browse-url))

(use-package eww
  :bind (("C-c w b" . eww-list-bookmarks)
         ("C-c w w" . eww)))

(use-package sx
  :ensure t
  :bind (("C-c w s" . sx-tab-frontpage)
         ("C-c w S" . sx-tab-newest)
         ("C-c w a" . sx-ask)))

;; Settings for sending mail via my mail server
(use-package sendmail
  :defer t
  :config (setq send-mail-function 'smtpmail-send-it))

(use-package message
  :defer t
  :config (setq message-send-mail-function 'smtpmail-send-it
                ;; Don't keep message buffers around
                message-kill-buffer-on-exit t))

(use-package smtpmail
  :defer t
  :config (setq smtpmail-smtp-server "vega.uberspace.de"
                smtpmail-smtp-service 587
                smtpmail-stream-type 'starttls
                smtpmail-smtp-user user-login-name))

;; IRC with ERC or rcirc
(use-package erc
  :defer t
  :config
  (progn
    ;; Default server and nick
    (setq erc-server "chat.freenode.net"
          erc-port 7000
          erc-nick "lunaryorn"
          erc-nick-uniquifier "_"
          ;; Never open unencrypted ERC connections
          erc-server-connect-function 'erc-open-tls-stream)

    ;; Spell-check ERC buffers
    (add-to-list 'erc-modules 'spelling)
    (erc-update-modules)))

(use-package erc-join
  :defer t
  :config
  ;; Standard channels on Freenode
  (setq erc-autojoin-channels-alist '(("\\.freenode\\.net" . ("#emacs")))))

(use-package erc-track
  :defer t
  :config
  ;; Switch to newest buffer by default, and don't ask before rebinding the keys
  (setq erc-track-switch-direction 'newest
        erc-track-enable-keybindings t))

(use-package rcirc
  :defer t
  :config
  (progn
    (setq rcirc-default-full-name (format "%s (http://www.lunaryorn.com)"
                                          user-full-name)
          rcirc-default-nick "lunaryorn"
          rcirc-time-format "%Y-%m-%d %H:%M "
          rcirc-server-alist
          '(("chat.freenode.not" :port 7000 :user-name "lunaryorn"
             :encryption tls :channels ("#emacs" "#haskell" "#hakyll" "#zsh"))))

    (add-hook 'rcirc-mode-hook #'flyspell-mode)

    (rcirc-track-minor-mode)))


;;; Online Help
(use-package find-func
  :defer t
  ;; Find function and variable definitions
  :idle (find-function-setup-keys)
  :idle-priority 10)

(bind-key "C-h A" #'apropos)


;;; Key bindings

(defvar lunaryorn-utilities-map nil
  "Keymap for various utilities.")

(define-prefix-command 'lunaryorn-utility 'lunaryorn-utilities-map)
(let ((map lunaryorn-utilities-map))
  (define-key map (kbd "d") #'lunaryorn-insert-current-date))

(lunaryorn-after 'lisp-mode
    (define-key emacs-lisp-mode-map (kbd "C-c f c") #'lunaryorn-find-cask-file))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
