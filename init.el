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
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Install all required packages if absent
(defconst lunaryorn-packages
  '(
    ;; Basic libraries
    dash                                ; List processing
    epl                                 ; Package environment
    exec-path-from-shell                ; Environment fixup
    ;; Color theme
    solarized-theme
    ;; UI improvements
    anzu                                ; Mode line indicators for isearch
    browse-kill-ring                    ; Kill ring browser
    smex                                ; Improved M-x
    fancy-battery                       ; Nice battery display
    nyan-mode                           ; Most awesome mode line extension evar
    ;; Buffer management
    ibuffer-vc                         ; Group and sort buffers by VC state
    ;; File handling
    ignoramus                           ; Ignore uninteresting files
    hardhat                             ; Protect user-writable files
    launch                              ; Open files externally
    ;; Navigation tools
    ido-ubiquitous                      ; Use IDO everywhere
    flx-ido                             ; Powerful flex matching for IDO
    imenu-anywhere                      ; imenu with IDO and for all buffers
    ido-vertical-mode                   ; Show IDO vertically
    ace-jump-mode                       ; Fast jump within the buffer
    ;; Editing indicators
    nlinum                              ; Line numbers in display margin
    page-break-lines                    ; page breaks
    volatile-highlights                 ; certain editing operations,
    flycheck                            ; and syntax errors
    ;; Editing helpers
    whitespace-cleanup-mode             ; Cleanup whitespace on save
    undo-tree                           ; Undo reloaded
    adaptive-wrap                       ; Automatic wrap prefix
    expand-region                       ; Expand region by semantic units
    multiple-cursors                    ; Multiple cursors
    easy-kill                           ; Killing and marking on steroids
    ;; Search and replace
    ag                                  ; Code search
    wgrep wgrep-ag                      ; Edit ag results in-place
    visual-regexp                       ; Regexp reloaded
    ;; Completion and expansion
    company                             ; Auto completion
    company-math                        ; Math characters for completion
    ;; LaTeX/AUCTeX
    auctex                              ; The one and only LaTeX environment
    auctex-latexmk                      ; latexmk support for AUCTeX
    ;; Markup languages
    markdown-mode                       ; Markdown major mode
    graphviz-dot-mode                   ; Graphviz mode
    ;; Configuration languages
    puppet-mode                         ; For Puppet files
    ;; YAML
    yaml-mode                           ; YAML major mode
    ansible-doc                         ; Ansible documentation lookup
    ;; General programming utilities
    highlight-symbol                    ; Symbol awareness
    pcre2el                             ; Regular expression utilities
    highlight-numbers                   ; Syntax highlighting for numeric
                                        ; literals
    rainbow-delimiters                  ; Color delimiters by level
    rainbow-mode                        ; Show colours as they are
    ;; Programming languages
    js2-mode                            ; Powerful Javascript mode
    feature-mode                        ; Cucumber major mode
    cmake-mode                          ; CMake files
    ;; Python
    anaconda-mode                       ; Documentation, lookup and navigation
    company-anaconda                    ; Company integration for Anaconda
    pip-requirements                    ; requirements.txt files
    ;; Ruby support
    inf-ruby                            ; Ruby interpreter in Emacs
    robe                                ; Code navigation, docs and completion
    ;; Rust
    rust-mode
    toml-mode                           ; For Cargo.toml
    flycheck-rust                       ; Better Rust support for Flycheck
    ;; Haskell support
    haskell-mode                        ; Haskell major modes
    ghci-completion                     ; Complete GHCI commands
    flycheck-haskell                    ; Improve Haskell syntax checking
    shm                                 ; Structured Haskell editing
    ;; OCaml support
    tuareg                              ; OCaml major mode
    merlin                              ; OCaml completion engine
    ;; Lisp tools
    paredit                            ; Balanced parenthesis editing
    ;; Emacs Lisp utility modes and libraries
    elisp-slime-nav                     ; Navigate to symbol definitions
    macrostep                           ; Interactively expand macros
    flycheck-cask                       ; Cask support for Flycheck
    ;; Clojure
    cider                               ; Clojure IDE
    clojure-mode-extra-font-locking
    ;; General Version Control
    diff-hl                             ; Highlight VCS diffs in the fringe
    ;; Git and Gist integration
    magit                               ; Git frontend
    git-commit-mode                     ; Git commit message mode
    gitconfig-mode                      ; Git configuration mode
    gitignore-mode                      ; .gitignore mode
    gitattributes-mode                  ; Git attributes mode
    git-rebase-mode                     ; Mode for git rebase -i
    git-timemachine                     ; Go back in (Git) time
    ;; Utilities
    projectile                          ; Project interaction
    google-this                         ; Google from Emacs
    paradox                             ; Better package menu
    )
  "Packages needed by my configuration.")

(defun lunaryorn-ensure-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package lunaryorn-packages)
    (unless (package-installed-p package)
      (package-install package))))

(lunaryorn-ensure-packages)


;;; Requires

(require 'dash)

(require 'subr-x)
(require 'rx)


;;; Package configuration and initialization

(defmacro lunaryorn-after (feature &rest forms)
  "After FEATURE is loaded, evaluate FORMS.

FORMS is byte compiled.

FEATURE may be a named feature or a file name, see
`eval-after-load' for details."
  (declare (indent 1) (debug t))
  (when (bound-and-true-p byte-compile-current-file)
    (if (stringp feature)
        (load feature nil 'no-error)
      (require feature nil 'no-error)))
  `(with-eval-after-load ',feature ,@forms))

(defun lunaryorn-auto-modes (&rest modes-and-patterns)
  "Add MODES-AND-PATTERNS to `auto-mode-alist'.

MODES-AND-PATTERNS is of the form `(mode1 pattern1 pattern2 …
mode2 pattern3 pattern4)'.  For each major mode symbol, add auto
mode entries for all subsequent patterns until the next major
mode symbol."
  (dolist (cell (-partition-by-header #'symbolp modes-and-patterns))
    (pcase-let ((`(,mode . ,patterns) cell))
      (dolist (pattern patterns)
        (add-to-list 'auto-mode-alist (cons pattern mode))))))

(defconst lunaryorn-font-lock-keywords
  `((,(rx "(" symbol-start
          (group (or "lunaryorn-after" "lunaryorn-auto-modes"))
          symbol-end
          (optional (one-or-more (syntax whitespace))
                    symbol-start
                    (group (one-or-more (or (syntax word) (syntax symbol))))
                    symbol-end))
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t)))
  "Our font lock keywords for Lisp modes.")

;; Teach Emacs Lisp modes about our keywords
(lunaryorn-after lisp-mode
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (font-lock-add-keywords mode lunaryorn-font-lock-keywords 'append)))

(lunaryorn-after ielm
  (font-lock-add-keywords 'inferior-emacs-lisp-mode
                          lunaryorn-font-lock-keywords 'append))


;;; Environment fixup
(lunaryorn-after exec-path-from-shell
  (when (string-match-p "/zsh$" (getenv "SHELL"))
    ;; Use a non-interactive shell.  We use a login shell, even though we have
    ;; our paths setup in .zshenv.  However, OS X adds global settings to the
    ;; login profile.  Notably, this affects /usr/texbin from MacTeX
    (setq exec-path-from-shell-arguments '("-l")))

  (dolist (var '("EMAIL" "PYTHONPATH" "INFOPATH"))
    (add-to-list 'exec-path-from-shell-variables var)))

(when (and (eq system-type 'darwin) (display-graphic-p))
  (exec-path-from-shell-initialize)

  (setq user-mail-address (getenv "EMAIL"))

  ;; Re-initialize the `Info-directory-list' from $INFOPATH.  Since package.el
  ;; already initializes info, we need to explicitly add the $INFOPATH
  ;; directories to `Info-directory-list'.
  (dolist (dir (parse-colon-path (getenv "INFOPATH")))
    (when dir
      (add-to-list 'Info-directory-list dir))))


;;; Customization interface
(defconst lunaryorn-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(lunaryorn-after cus-edit
  (setq custom-file lunaryorn-custom-file))

(load lunaryorn-custom-file 'no-error 'no-message)


;;; OS X support

(lunaryorn-after ns-win
  (setq ns-pop-up-frames nil            ; Don't pop up new frames from the
                                        ; workspace
        mac-option-modifier 'meta       ; Option is simply the natural Meta
        mac-command-modifier 'meta      ; But command is a lot easier to hit
        mac-right-command-modifier 'left
        mac-right-option-modifier 'none ; Keep right option for accented input
        ;; Just in case we ever need these keys
        mac-function-modifier 'hyper))

;; Prefer GNU utilities over the BSD variants in Emacs, because the GNU tools
;; integrate better with Emacs
(defconst lunaryorn-gnu-ls (and (eq system-type 'darwin) (executable-find "gls"))
  "Path to GNU ls on OS X.")

(lunaryorn-after files
  (when lunaryorn-gnu-ls
    ;; Use GNU ls if available
    (setq insert-directory-program lunaryorn-gnu-ls)))

(lunaryorn-after dired
  (when (and (eq system-type 'darwin) (not lunaryorn-gnu-ls))
    ;; Don't probe for --dired flag in Dired, because we already know that GNU
    ;; ls is missing!
    (setq dired-use-ls-dired nil)))

(lunaryorn-after grep
  ;; Use GNU find on OS X, if possible
  (when-let (gfind (and (eq system-type 'darwin) (executable-find "gfind")))
    (setq find-program gfind)))

(lunaryorn-after locate
  ;; Use mdfind as locate substitute on OS X, to utilize the Spotlight database
  (when-let (mdfind (and (eq system-type 'darwin) (executable-find "mdfind")))
    (setq locate-command mdfind)))

;; Trash support for OS X.  On OS X, Emacs doesn't support the system trash
;; properly, so we try to work around it by providing our own trashing function.
;; If that fails, disable trashing and warn!
(defconst lunaryorn-darwin-trash-tool "trash"
  "A CLI tool to trash files.")

(defun lunaryorn-darwin-move-file-to-trash (file)
  "Move FILE to trash on OS X."
  (call-process lunaryorn-darwin-trash-tool nil nil nil (expand-file-name file)))

(if (executable-find lunaryorn-darwin-trash-tool)
    (defalias 'system-move-file-to-trash 'lunaryorn-darwin-move-file-to-trash)
  (warn "Trash support not available!
Install Trash from https://github.com/ali-rantakari/trash!
Homebrew: brew install trash"))

;; Utility functions for OS X
(defun lunaryorn-id-of-bundle (bundle)
  "Get the ID of a BUNDLE.

BUNDLE is the user-visible name of the bundle as string.  Return
the id of the bundle as string.

These bundle IDs are normally constant.  Thus you may use this
function to determine the ID once, and then hard-code it in your
code."
  (let ((script (format "id of app \"%s\"" bundle)))
    (car (process-lines "osascript" "-e" script))))

(defun lunaryorn-path-of-bundle (id)
  "Get the path of a bundle with ID.

ID is the bundle ID (see `lunaryorn-id-of-bundle' as string.  Return
the directory path of the bundle as string."
  (let ((query (format "kMDItemCFBundleIdentifier == '%s'" id)))
    (car (process-lines "mdfind" query))))

(defun lunaryorn-homebrew-prefix (&optional formula)
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

(defun lunaryorn-homebrew-installed-p (&optional formula)
  "Determine whether a homebrew FORMULA is installed.

Without FORMULA determine whether Homebrew itself is available."
  (if formula
      (when (lunaryorn-homebrew-prefix formula) t)
    (when (executable-find "brew") t)))


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
(setq ring-bell-function 'ignore
      inhibit-startup-screen t
      initial-scratch-message nil)
(fset 'yes-or-no-p 'y-or-n-p)
;; Opt out from the startup message in the echo area by simply disabling this
;; ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message 'ignore)

;;; Font setup
(let ((font "Source Code Pro")
      (size (pcase system-type
              (`darwin 13)
              (_ 10))))
  (if (x-family-fonts font)
      (set-frame-font (format "%s-%s" font size) nil t)
    (lwarn 'emacs :warning "%S font is missing!" font)))

(lunaryorn-after solarized
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

(load-theme 'solarized-light 'no-confirm)


;;; The mode line

;; Standard stuff
(line-number-mode)
(column-number-mode)
(size-indication-mode)
(fancy-battery-mode)                    ; Battery status

;; Indicate position/total matches for incremental searches in the mode line
(global-anzu-mode)
(lunaryorn-after anzu
  ;; Please don't mess with my mode line
  (setq anzu-cons-mode-line-p nil))

;; Use an awesome cat to indicate the buffer position in the mode line!
(lunaryorn-after nyan-mode
  ;; Reduce size of nyan bar.  My mode line is too small for the whole thing in
  ;; its unshortened awesomeness!
  (setq nyan-bar-length 16))
(autoload 'nyan-create "nyan-mode")     ; Nyan is missing autoloads

;; Show the current function name in the mode line
(lunaryorn-after which-func
  (setq which-func-unknown "⊥" ; The default is really boring…
        which-func-format
        `("["
          (:propertize (:eval (lunaryorn-which-func-current))
                       local-map ,which-func-keymap
                       face which-func
                       mouse-face mode-line-highlight
                       help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end")
          "]")
        ))

(defun lunaryorn-current-namespace ()
  "Determine the namespace of the current file."
  (when-let (filename (buffer-file-name))
    (if (string= (file-truename filename) (file-truename user-init-file))
        "lunaryorn"                       ; The “namespace” of my init
      (file-name-base filename))))

(defun lunaryorn-which-func-current ()
  "Determine the name of the current function."
  (if-let (current (or (gethash (selected-window) which-func-table)))
      (truncate-string-to-width
       (pcase major-mode
         (`emacs-lisp-mode
          (let ((namespace (lunaryorn-current-namespace)))
            (if (and namespace
                     (string-prefix-p namespace current 'ignore-case))
                (concat "…" (substring current (length namespace)))
              current)))
         (`latex-mode
          ;; In AUCTeX' LaTeX Mode, strip leading spaces from headings, and
          ;; replace them with a number indicating the heading level
          (if (string-match (rx string-start (group (1+ blank))) current)
              (let ((level (length (match-string 1 current))))
                (format "(%i)%s" level (substring current level)))
            current))
         (_ current))
       20 nil nil "…")
    which-func-unknown))

(which-function-mode)

;; Improve our mode line
(defvar lunaryorn-vc-mode-line
  '(" " (:propertize
         ;; Strip the backend name from the VC status information
         (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
                  (substring vc-mode (+ (length backend) 2))))
         face font-lock-variable-name-face))
  "Mode line format for VC Mode.")
(put 'lunaryorn-vc-mode-line 'risky-local-variable t)

(setq-default mode-line-format
              '("%e" mode-line-front-space
                ;; Standard info about the current buffer
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                ;; Some specific information about the current buffer:
                (paredit-mode (:propertize " ()" face bold))
                (projectile-mode projectile-mode-line)
                (vc-mode lunaryorn-vc-mode-line)   ; VC information
                (flycheck-mode flycheck-mode-line) ; Flycheck status
                (anzu-mode (:eval                  ; isearch pos/matches
                            (when (> anzu--total-matched 0)
                              (concat " " (anzu--update-mode-line)))))
                (multiple-cursors-mode mc/mode-line) ; Number of cursors
                ;; Misc information, notably battery state and function name
                " "
                mode-line-misc-info
                ;; And the modes, which we don't really care for anyway
                " " mode-line-modes mode-line-end-spaces)
              mode-line-position
              '((:eval (nyan-create))
                " "
                (line-number-mode
                 ("%l" (column-number-mode ":%c"))))
              mode-line-remote
              '(:eval
                (when-let (host (file-remote-p default-directory 'host))
                  (propertize (concat "@" host) 'face
                              '(italic warning))))
              mode-line-modified
              '(:eval
                (cond (buffer-read-only (propertize "X" 'face 'warning))
                      ((buffer-modified-p) (propertize "✸" 'face 'error))
                      (t (propertize "⛃" 'face 'success)))))


;;; The minibuffer

;; Increase Emacs' memory of the past
(setq history-length 1000)

;; Save a minibuffer input history
(lunaryorn-after savehist
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval 180))
(savehist-mode t)

;; Boost file and buffer operations by flexible matching and the ability to
;; perform operations like deleting files or killing buffers directly from the
;; minibuffer
(lunaryorn-after ido
  (setq ido-enable-flex-matching t      ; Match characters if string doesn't
                                        ; match
        ido-create-new-buffer 'always   ; Create a new buffer if nothing matches
        ido-use-filename-at-point 'guess
        ;; Visit buffers and files in the selected window
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        ido-use-faces nil))             ; Prefer flx ido faces

(ido-mode t)                            ; Enable IDO,…
(ido-everywhere)                        ; …everywhere…
(ido-ubiquitous-mode)                   ; …really!
(flx-ido-mode)                          ; Powerful IDO flex matching
(ido-vertical-mode)                     ; Show IDO completions vertically

;; Configure Smex
(lunaryorn-after smex
  (setq smex-save-file (locate-user-emacs-file ".smex-items")))

;; Tune `eval-expression'
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)


;;; Buffer, Windows and Frames

;; Make uniquify rename buffers like in path name notation
(lunaryorn-after uniquify
  (setq uniquify-buffer-name-style 'forward))

;; Clean stale buffers
(require 'midnight)

;; Don't kill the important buffers
(defconst lunaryorn-do-not-kill-buffer-names '("*scratch*" "*Messages*")
  "Names of buffers that should not be killed.")

(defun lunaryorn-do-not-kill-important-buffers ()
  "Inhibit killing of important buffers.

Add this to `kill-buffer-query-functions'."
  (if (not (member (buffer-name) lunaryorn-do-not-kill-buffer-names))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))

(add-hook 'kill-buffer-query-functions #'lunaryorn-do-not-kill-important-buffers)

(lunaryorn-after ibuffer
  ;; Group ibuffer by VC status
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))

  ;; Show VC Status in ibuffer
  (setq ibuffer-formats
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

;; Move between windows with Shift + Arrow keys
(windmove-default-keybindings)

;; Undo and redo window configurations with C-c Left and C-c Right respectively
(winner-mode)

;; Prevent Ediff from spamming the frame
(lunaryorn-after ediff-wind
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

;; Save buffers, windows and frames
(lunaryorn-after desktop
  ;; Don't autosave desktops, it's too expensive.  Desktops aren't that
  ;; precious, and Emacs will save the desktop on exit anyway.
  (setq desktop-auto-save-timeout nil
        desktop-modes-not-to-save '(magit-mode)))
(desktop-save-mode)


;;; File handling

;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Autosave buffers when focus is lost, see
;; http://emacsredux.com/blog/2014/03/22/a-peek-at-emacs-24-dot-4-focus-hooks/
(defun lunaryorn-force-save-some-buffers ()
  "Save all modified buffers, without prompts."
  (save-some-buffers 'dont-ask))
(add-hook 'focus-out-hook #'lunaryorn-force-save-some-buffers)

;; Delete files to trash
(setq delete-by-moving-to-trash
      (or (not (eq system-type 'darwin)) ; Trash is well supported on other
                                        ; systems
          (fboundp 'system-move-file-to-trash)))

;; Store Tramp auto save files locally
(lunaryorn-after tramp
  (setq tramp-auto-save-directory (locate-user-emacs-file "tramp-auto-save")))

(lunaryorn-after dired
  ;; Power up dired
  (require 'dired-x)

  ;; Always revert Dired buffers on revisiting
  (setq dired-auto-revert-buffer t
        dired-listing-switches "-alh"  ; Human-readable sizes by default
        ))

(lunaryorn-after dired-x
  (when (eq system-type 'darwin)
    ;; OS X bsdtar is mostly compatible with GNU Tar
    (setq dired-guess-shell-gnutar "tar")))

;; Update copyright when visiting files
(add-hook 'find-file-hook 'copyright-update)

;; Ignore uninteresting files
(ignoramus-setup)

;; Do not clobber user writeable files
(lunaryorn-after hardhat
  ;; Add local homebrew prefix to the list of protected directories.  Hardhat
  ;; itself only handles /usr/local/
  (when (eq system-type 'darwin)
    (when-let (prefix (lunaryorn-homebrew-prefix))
      (add-to-list 'hardhat-fullpath-protected-regexps prefix))))
(global-hardhat-mode)

;; Save bookmarks immediately after a bookmark was added
(lunaryorn-after bookmark
  (setq bookmark-save-flag 1))

;; Track recent files
(lunaryorn-after recentf
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 300))
(recentf-mode t)

;; Open recent files with IDO, see
;; http://emacsredux.com/blog/2013/04/05/recently-visited-files/
(lunaryorn-after recentf
  (defun lunaryorn-ido-find-recentf ()
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

(defun lunaryorn-launch-dired-dwim ()
  "Open the marked files externally.

If no files are marked, open the current directory instead."
  (let ((marked-files (dired-get-marked-files)))
    (if marked-files
        (launch-files marked-files :confirm)
      (launch-directory (dired-current-directory)))))

(defun lunaryorn-launch-dwim ()
  "Open the current file externally."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (lunaryorn-launch-dired-dwim)
    (if (buffer-file-name)
        (launch-file (buffer-file-name))
      (user-error "The current buffer is not visiting a file"))))

;; Utility commands for working with files, see:
;; http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
;; http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
;; https://github.com/bbatsov/prelude/blob/master/core/prelude-core.el
(defun lunaryorn-current-file ()
  "Gets the \"file\" of the current buffer.

The file is the buffer's file name, or the `default-directory' in
`dired-mode'."
  (if (eq major-mode 'dired-mode)
      default-directory
    (buffer-file-name)))

(defun lunaryorn-copy-filename-as-kill (&optional arg)
  "Copy the name of the currently visited file to kill ring.

With a zero prefix arg, copy the absolute file name.  With
\\[universal-argument], copy the file name relative to the
current buffer's `default-directory'.  Otherwise copy the
non-directory part only."
  (interactive "P")
  (-if-let* ((filename (lunaryorn-current-file))
             (name-to-copy (cond ((zerop (prefix-numeric-value arg)) filename)
                                 ((consp arg) (file-relative-name filename))
                                 (:else (file-name-nondirectory filename)))))
    (progn
      (kill-new name-to-copy)
      (message "%s" name-to-copy))
    (user-error "This buffer is not visiting a file")))

(defun lunaryorn-rename-file-and-buffer ()
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

(defun lunaryorn-delete-file-and-buffer ()
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
(defun lunaryorn-find-user-init-file-other-window ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))


;;; Basic editing
;; Make `kill-whole-line' indentation aware
(defun lunaryorn-smart-kill-whole-line (&optional arg)
  "Kill whole line and move back to indentation.

Kill the whole line with function `kill-whole-line' and then move
`back-to-indentation'."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))

;; Some other utilities
(defun lunaryorn-smart-backward-kill-line ()
  "Kill line backwards and re-indent."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

(defun lunaryorn-smart-open-line ()
  "Insert empty line after the current line."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

;; Make C-a toggle between beginning of line and indentation
(defun lunaryorn-back-to-indentation-or-beginning-of-line (arg)
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

;; Electric pairing and code layout
(electric-pair-mode)
(electric-layout-mode)

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(setq indicate-empty-lines t
      require-final-newline t)

;; Highlight bad whitespace
(lunaryorn-after whitespace
  ;; Highlight tabs, empty lines at beg/end, trailing whitespaces and overlong
  ;; portions of lines via faces.  Also indicate tabs via characters
  (setq whitespace-style '(face indentation space-after-tab space-before-tab
                                tab-mark empty trailing lines-tail)
        whitespace-line-column nil))    ; Use `fill-column' for overlong lines

;; A function to disable highlighting of long lines in modes
(lunaryorn-after whitespace
  (defun lunaryorn-whitespace-style-no-long-lines ()
    "Configure `whitespace-mode' for Org.

Disable the highlighting of overlong lines."
    (setq-local whitespace-style (-difference whitespace-style
                                              '(lines lines-tail)))
    (when whitespace-mode
      (whitespace-mode -1)
      (whitespace-mode 1))))

;; Clean up whitespace
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'whitespace-mode)
  (add-hook hook #'whitespace-cleanup-mode))

;; Delete the selection instead of inserting
(delete-selection-mode)

(setq kill-ring-max 200                 ; More killed items
      ;; Save the contents of the clipboard to kill ring before killing
      save-interprogram-paste-before-kill t)

;; Configure a reasonable fill column, indicate it in the buffer and enable
;; automatic filling
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(define-minor-mode lunaryorn-auto-fill-comments-mode
  "Minor mode to auto-fill comments only."
  :lighter nil
  :keymap nil
  (cond
   (lunaryorn-auto-fill-comments-mode
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode 1))
   (:else
    (kill-local-variable 'comment-auto-fill-only-comments)
    (auto-fill-mode -1))))
(add-hook 'prog-mode-hook 'lunaryorn-auto-fill-comments-mode)

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

;; Highlight the current line, editing operations, and match parens in the
;; buffer
(global-hl-line-mode 1)
(require 'volatile-highlights)          ; Doesn't autoload :|
(volatile-highlights-mode t)

;; Highlight delimiters…
(show-paren-mode)                       ; … by matching delimiters
(lunaryorn-after paren
  ;; Show parenthesis more aggressively
  (setq show-paren-style 'mixed
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))
(dolist (hook '(text-mode-hook prog-mode-hook)) ; … by depth
  (add-hook hook #'rainbow-delimiters-mode))
;; Add custom highlights to buffers
(global-hi-lock-mode 1)

;; Jump to characters in buffers
(lunaryorn-after ace-jump-mode
  ;; Sync marks with Emacs built-in commands
  (ace-jump-mode-enable-mark-sync))

;; Power up undo
(global-undo-tree-mode)

;; Nicify page breaks
(global-page-break-lines-mode)

;; Outline commands
(dolist (hook '(text-mode-hook prog-mode-hook))
  (add-hook hook #'outline-minor-mode))

;; An Emacs server for `emacsclient'
(require 'server)
(unless (server-running-p) (server-start))


;;; Multiple cursors
(lunaryorn-after multiple-cursors-core
  (setq mc/mode-line
        ;; Simplify the MC mode line indicator
        '(:propertize (:eval (concat " " (number-to-string (mc/num-cursors))))
                      face font-lock-warning-face)))


;;; Completion and expansion

;; Configure hippie-expand reasonably
(lunaryorn-after hippie-exp
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

;; In `completion-at-point', do not pop up silly completion buffers for less
;; than five candidates.  Cycle instead.
(setq completion-cycle-threshold 5)

;; Enable auto-completion
(lunaryorn-after company
  (setq company-tooltip-align-annotations t
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t)

  ;; Add backend for math characters
  (add-to-list 'company-backend 'company-math-symbols-unicode))
(global-company-mode)


;;; Spell checking

(lunaryorn-after ispell
  (setq ispell-program-name (if (eq system-type 'darwin)
                                (executable-find "aspell")
                              (executable-find "hunspell"))
        ispell-dictionary "en_GB"       ; Default dictionnary
        ispell-silently-savep t         ; Don't ask when saving the private dict
        ;; Increase the height of the choices window to take our header line
        ;; into account.
        ispell-choices-win-default-height 5)

  (unless ispell-program-name
    (warn "No spell checker available.  Install Hunspell or ASpell for OS X.")))

(lunaryorn-after flyspell
  ;; Free M-Tab and C-M-i, and never take it again!
  (define-key flyspell-mode-map "\M-\t" nil)
  (setq flyspell-use-meta-tab nil
        ;; Make Flyspell less chatty
        flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil))

(dolist (hook '(text-mode-hook message-mode-hook))
  (add-hook hook 'turn-on-flyspell))
(add-hook 'prog-mode-hook 'flyspell-prog-mode)


;;; Syntax checking

;; On the fly syntax checking
(lunaryorn-after flycheck
  (defun lunaryorn-flycheck-mode-line-status ()
    "Create a mode line status text for Flycheck."
    (let* ((menu (mouse-menu-non-singleton flycheck-mode-menu-map))
           (map (make-mode-line-mouse-map 'mouse-1
                                          (lambda ()
                                            (interactive)
                                            (popup-menu menu))))
           (text-and-face
            (pcase flycheck-last-status-change
              (`not-checked nil)
              (`no-checker '(" -" . warning))
              (`running '( " ✸" . success))
              (`errored '( " !" . error))
              (`finished
               (let* ((error-counts (flycheck-count-errors
                                     flycheck-current-errors))
                      (no-errors (cdr (assq 'error error-counts)))
                      (no-warnings (cdr (assq 'warning error-counts)))
                      (face (cond (no-errors 'error)
                                  (no-warnings 'warning)
                                  (t 'success))))
                 (cons (format " %s/%s" (or no-errors 0) (or no-warnings 0))
                       face)))
              (`interrupted (cons " -" nil))
              (`suspicious '(" ?" . warning)))))
      (when text-and-face
        (propertize (car text-and-face) 'face (cdr text-and-face)
                    'mouse-face 'mode-line-highlight
                    'local-map map))))

  (setq flycheck-completion-system 'ido
        flycheck-mode-line
        '(:eval (lunaryorn-flycheck-mode-line-status))
        flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list)

  ;; Use italic face for checker name
  (set-face-attribute 'flycheck-error-list-checker-name nil :inherit 'italic))
(global-flycheck-mode)


;;; LaTeX with AUCTeX

(require 'tex-site nil :no-error)

;; TeX editing
(lunaryorn-after tex
  (setq TeX-parse-self t                ; Parse documents to provide completion
                                        ; for packages, etc.
        TeX-auto-save t                 ; Automatically save style information
        TeX-electric-sub-and-superscript t ; Automatically insert braces after
                                           ; sub- and superscripts in math mode
        ;; Don't insert magic quotes right away.
        TeX-quote-after-quote t)

  (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(lunaryorn-after tex-style
  ;; Enable support for csquotes
  (setq LaTeX-csquotes-close-quote "}"
        LaTeX-csquotes-open-quote "\\enquote{"))

(lunaryorn-after latex
  ;; Teach TeX folding about KOMA script sections
  (setq TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
                            (,(rx (0+ space) "\\subsection*{") 3)
                            (,(rx (0+ space) "\\subsubsection*{") 4)
                            (,(rx (0+ space) "\\minisec{") 5))
        ;; No language-specific hyphens please
        LaTeX-babel-hyphen nil)

  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode) ; Easy math input
  (add-hook 'LaTeX-math-mode #'LaTeX-preview-setup)  ; LaTeX in-buffer previews
  (add-hook 'LaTeX-mode-hook #'reftex-mode) ; Cross-refs on steroids
  )

(lunaryorn-after tex-mode
  (font-lock-add-keywords 'latex-mode
                          `((,(rx "\\"
                                  symbol-start
                                  "fx" (1+ (or (syntax word) (syntax symbol)))
                                  symbol-end)
                             . font-lock-warning-face))))

(lunaryorn-after company
  (defun lunaryorn-add-latex-backends ()
    "Add additional Company backends for LaTeX."
    (setq-local company-backends
                (cons #'company-math-symbols-latex
                      (cons #'company-latex-commands company-backends))))

  (add-hook 'LaTeX-mode-hook 'lunaryorn-add-latex-backends))

;;;; TeX processing settings
(lunaryorn-after tex-buf
  ;; Don't ask for confirmation when saving before processing
  (setq TeX-save-query nil))

(lunaryorn-after tex
  (setq TeX-clean-confirm nil           ; Don't ask for confirmation when
                                        ; cleaning
        ;; Provide forward and inverse search with SyncTeX
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex)
  (setq-default TeX-master nil          ; Ask for the master file
                TeX-engine 'luatex      ; Use a modern engine
                TeX-PDF-mode t)         ; Create PDFs by default

  ;; Replace the rotten Lacheck with Chktex
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s"))

(lunaryorn-after latex
  ;; Cleanup files from unusual packages
  (dolist (ext '("\\.lox"               ; Lists from the fixme package
                 "\\.lol"               ; Lists from the listings package
                 "\\.cb" "\\.cb2"       ; Locations from the changebar package
                 ))
    (add-to-list 'LaTeX-clean-intermediate-suffixes ext))

  ;; Add support for latexmk
  (auctex-latexmk-setup))

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

(lunaryorn-after tex
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

;; Configure BibTeX
(lunaryorn-after bibtex
  (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

  ;; Use a modern BibTeX dialect
  (bibtex-set-dialect 'biblatex))

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
(lunaryorn-after reftex
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

;; Plug reftex into bib-cite
(lunaryorn-after bib-cite
  (setq bib-cite-use-reftex-view-crossref t)) ; Plug into bibcite


;;; ReStructuredText editing
(lunaryorn-after rst
  ;; Indent with 3 spaces after all kinds of literal blocks
  (setq rst-indent-literal-minimized 3
        rst-indent-literal-normal 3)

  (let ((map rst-mode-map))
    ;; Free C-= for `expand-region'. `rst-adjust' is still on C-c C-= and C-c
    ;; C-a C-a
    (define-key map (kbd "C-=") nil)
    ;; For similarity with AUCTeX
    (define-key map (kbd "C-c C-j") #'rst-insert-list)))


;;; Markdown editing

;; Why doesn't Markdown Mode do this itself?!
(lunaryorn-auto-modes 'markdown-mode (rx "." (or "md" "markdown") string-end))

(lunaryorn-after markdown-mode
  ;; Use Pandoc to process Markdown
  (setq markdown-command "pandoc -s -f markdown -t html5")

  ;; No filling and no indicator in GFM mode, because line breaks are
  ;; significant
  (add-hook 'gfm-mode-hook #'turn-off-fci-mode)
  (add-hook 'gfm-mode-hook #'turn-off-auto-fill)
  ;; Use visual lines instead
  (add-hook 'gfm-mode-hook #'visual-line-mode)

  (lunaryorn-after whitespace
    (add-hook 'gfm-mode-hook #'lunaryorn-whitespace-style-no-long-lines)))


;;; YAML

(lunaryorn-after yaml-mode
  ;; Whitespace handling and filling
  (add-hook 'yaml-mode-hook #'whitespace-mode)
  (add-hook 'yaml-mode-hook #'whitespace-cleanup-mode)
  (add-hook 'yaml-mode-hook #'lunaryorn-auto-fill-comments-mode)
  (add-hook 'yaml-mode-hook #'fci-mode)
  (add-hook 'yaml-mode-hook #'flyspell-prog-mode) ; Spell checking
  (add-hook 'yaml-mode-hook #'ansible-doc-mode) ; Ansible documentation lookup
  )


;;; Graphviz

(lunaryorn-after graphviz-dot-mode
  (setq graphviz-dot-indent-width 4     ; Reduce indentation
        ))


;;; Configuration languages
(lunaryorn-after puppet-mode
  ;; Fontify variables in Puppet comments
  (setq puppet-fontify-variables-in-comments t))


;;; Symbol “awareness”

;; Navigate occurrences of the symbol under point with M-n and M-p
(add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)

;; Highlight the symbol under point
(lunaryorn-after highlight-symbol
  (setq highlight-symbol-idle-delay 0.4 ; Highlight almost immediately
        highlight-symbol-on-navigation-p t) ; Highlight immediately after
                                            ; navigation
  )
(add-hook 'prog-mode-hook #'highlight-symbol-mode)


;;; Programming utilities

;; Compilation from Emacs
(defun lunaryorn-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer.

Taken from http://stackoverflow.com/a/3072831/355252."
  (interactive)
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(lunaryorn-after compile
  (setq compilation-ask-about-save nil  ; Just save before compiling
        compilation-always-kill t       ; Just kill old compile processes before
                                        ; starting the new one
        compilation-scroll-output 'first-error ; Automatically scroll to first
                                               ; error
        )

  ;; Colorize output of Compilation Mode, see
  ;; http://stackoverflow.com/a/3072831/355252
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook #'lunaryorn-colorize-compilation-buffer))

;; Font lock for numeric literals
(add-hook 'prog-mode-hook #'highlight-numbers-mode)


;;; Emacs Lisp

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

(defconst lunaryorn-imenu-generic-expression
  `(("Lunaryorn packages"
     ,(rx line-start (zero-or-more (syntax whitespace))
          "(lunaryorn-after" (one-or-more (syntax whitespace))
          (group-n 1 (one-or-more (or (syntax word)
                                      (syntax symbol))))) 1))
  "IMenu index expression for my config.")

(defun lunaryorn-emacs-lisp-current-feature ()
  "Return the feature provided by the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp (rx line-start "(provide '"))
      (symbol-name (symbol-at-point)))))

;; Teach Emacs about Emacs scripts and Cask/Carton files
(add-to-list 'interpreter-mode-alist '("emacs" . emacs-lisp-mode))
(lunaryorn-auto-modes 'emacs-lisp-mode (rx "/" (or "Cask" "Carton") string-end))

(lunaryorn-after lisp-mode
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  ;; Check doc conventions when eval'ing expressions
  (add-hook 'emacs-lisp-mode-hook #'checkdoc-minor-mode)

  ;; Add our IMenu index keywords
  (defun lunaryorn-emacs-lisp-setup-imenu ()
    "Add my IMenu index keywords."
    (setq imenu-generic-expression
          (append imenu-generic-expression lunaryorn-imenu-generic-expression)))

  (add-hook 'emacs-lisp-mode-hook #'lunaryorn-emacs-lisp-setup-imenu)

  ;; Load ERT to support unit test writing and running
  (require 'ert)

  ;; Load Dash and enable font-locking for its special forms
  (require 'dash)
  (dash-enable-font-lock))

;; Helpers for Emacs Lisp regexps
(rxt-global-mode)                       ; Powerful explanation and conversion
                                        ; functions for regular expressions in
                                        ; the C-c / map

(lunaryorn-after flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(lunaryorn-after ielm
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  ;; Enable lexical binding in IELM
  (add-hook 'ielm-mode-hook (lambda () (setq lexical-binding t))))

;; Hippie expand for Emacs Lisp
(lunaryorn-after hippie-exp
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

  (lunaryorn-after lisp-mode
    (dolist (hook '(emacs-lisp-mode-hook lisp-interaction-mode-hook))
      (add-hook hook #'lunaryorn-emacs-lisp-setup-hippie-expand)))

  (lunaryorn-after ielm
    (add-hook 'ielm-mode-hook #'lunaryorn-emacs-lisp-setup-hippie-expand)))


;;; Clojure

(lunaryorn-after clojure-mode
  ;; Extra font-locking for Clojure
  (require 'clojure-mode-extra-font-locking)

  (add-hook 'clojure-mode-hook #'paredit-mode))

(lunaryorn-after cider-mode
  (add-hook 'cider-mode-hook #'cider-turn-on-eldoc-mode))

(lunaryorn-after nrepl-client
  (setq nrepl-hide-special-buffers t))

(lunaryorn-after cider-repl
  (add-hook 'cider-repl-mode-hook #'paredit-mode)

  ;; Increase the history size and make it permanent
  (setq cider-repl-history-size 1000
        cider-repl-history-file (locate-user-emacs-file "cider-repl-history")))


;;; Python

;; Fill according to PEP 8
(defun lunaryorn-python-filling ()
  "Configure filling for Python."
  ;; PEP 8 recommends a maximum of 79 characters
  (setq fill-column 79))

(lunaryorn-after python
  ;; PEP 8 compliant filling rules
  (add-hook 'python-mode-hook #'lunaryorn-python-filling)
  (add-hook 'python-mode-hook #'subword-mode)
  ;; Lookup, navigation and completion
  (add-hook 'python-mode-hook #'anaconda-mode)
  ;; Pick Flycheck executables from current virtualenv automatically
  (add-hook 'python-mode-hook #'lunaryorn-flycheck-setup-python)

  ;; Use a decent syntax and style checker
  (setq python-check-command "pylint")

  ;; Use IPython as interpreter.  Stolen from Elpy
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i")

  (lunaryorn-after flycheck
    (defun lunaryorn-flycheck-setup-python-executables ()
      "Setup Python executables based on the current virtualenv."
      (let ((exec-path (python-shell-calculate-exec-path)))
        (setq flycheck-python-pylint-executable
              (executable-find "pylint"))))

    (defun lunaryorn-flycheck-setup-python ()
      "Setup Flycheck in Python buffers."
      (add-hook 'hack-local-variables-hook
                #'lunaryorn-flycheck-setup-python-executables 'local))))

(lunaryorn-after company
  (add-to-list 'company-backends 'company-anaconda)

  ;; Remove redundant company-ropemacs backend.  company-anaconda is superior.
  (setq company-backends (delq 'company-ropemacs company-backends)))

;; Requirements files
(add-to-list 'auto-mode-alist '("requirements\\.txt\\'" . pip-requirements-mode))


;;; Ruby
(lunaryorn-after ruby-mode
  ;; Setup inf-ruby and Robe
  (add-hook 'ruby-mode-hook #'robe-mode)
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)

  ;; Easily switch to Inf Ruby from compilation modes to Inf Ruby
  (inf-ruby-switch-setup))

(lunaryorn-after company
  (add-to-list 'company-backends 'company-robe))


;;; Rust
(lunaryorn-after flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


;;; Haskell
(lunaryorn-after haskell-mode
  ;; We need the following tools for our Haskell setup:
  ;;
  ;; cabal install hasktags structured-haskell-mode haskell-stylish
  (add-hook 'haskell-mode-hook #'subword-mode)     ; Subword navigation
  (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode) ; Scan and navigate
                                        ; declarations
  ;; Insert module templates into new buffers
  (add-hook 'haskell-mode-hook #'haskell-auto-insert-module-template)
  ;; Indentation and navigation on steroids
  (add-hook 'haskell-mode-hook #'structured-haskell-mode)
  ;; Better interactive repl, including Cabal project management
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)

  ;; Automatically run hasktags
  (setq haskell-tags-on-save t))

(lunaryorn-after haskell-cabal
  (add-hook 'haskell-cabal-mode #'interactive-haskell-mode))

(lunaryorn-after shm
  (require 'shm-case-split))

(lunaryorn-after inf-haskell
  (add-hook 'inferior-haskell-mode-hook #'turn-on-ghci-completion)
  (add-hook 'inferior-haskell-mode-hook #'subword-mode))

(lunaryorn-after haskell-interactive-mode
  (add-hook 'haskell-interactive-mode-hook #'turn-on-ghci-completion)
  (add-hook 'haskell-interactive-mode-hook #'subword-mode))

(lunaryorn-after haskell-process
  ;; Suggest adding/removing imports as by GHC warnings and Hoggle/GHCI loaded
  ;; modules respectively
  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-use-presentation-mode t ; Don't clutter the echo area
        haskell-process-show-debug-tips nil ; Disable tips
        haskell-process-log t           ; Log debugging information
        ;; Suggest imports automatically with Hayoo.  Hayoo is slower because
        ;; it's networked, but it covers all of hackage, which is really an
        ;; advantage.
        haskell-process-suggest-hoogle-imports nil
        haskell-process-suggest-hayoo-imports t
        ))

(lunaryorn-after flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))


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

(lunaryorn-after tuareg
  ;; Disable SMIE indentation in Tuareg.  It's just broken currently…
  (setq tuareg-use-smie nil)

  ;; Enable advanced completion engine for OCaml
  (add-hook 'tuareg-mode-hook #'merlin-mode))

(lunaryorn-after merlin
  (setq merlin-command 'opam))          ; Use Merlin from current OPAM env


;;; Shell scripting

;; Teach Emacs about Zsh scripts
(lunaryorn-auto-modes 'sh-mode (rx ".zsh" string-end))

;; Shell script indentation styles
(lunaryorn-after sh-script
  ;; Use two spaces in shell scripts.
  (setq sh-indentation 2                ; The basic indentation
        sh-basic-offset 2               ; The offset for nested indentation
        ))


;;; Misc programming languages

;; Javascript: Indentation
(lunaryorn-after js2-mode
  (setq-default js2-basic-offset 2))

(lunaryorn-auto-modes 'js2-mode (rx "." (or "js" "json") string-end))

;; XML: Complete closing tags, and insert XML declarations into empty files
(lunaryorn-after nxml-mode
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t))

;; Feature Mode
(lunaryorn-after feature-mode
  ;; Add standard hooks for Feature Mode, since it is no derived mode
  (add-hook 'feature-mode-hook #'whitespace-mode)
  (add-hook 'feature-mode-hook #'whitespace-cleanup-mode)
  (add-hook 'feature-mode-hook #'flyspell-mode))


;;; Proof General
(eval-and-compile
  ;; Load ProofGeneral if present
  (load (locate-user-emacs-file "vendor/ProofGeneral/generic/proof-site")
        'no-error))

(when (boundp 'proof-general-version)
  ;; If Proof General is loaded, do some additional setup
  (add-to-list 'Info-directory-list (expand-file-name "doc/"))
  (when (eq system-type 'darwin)
    (let ((isabelle-bundle (lunaryorn-path-of-bundle "de.tum.in.isabelle")))
      (when (and isabelle-bundle
                 (not (executable-find "isabelle_process")))
        (warn "Isabelle not in `exec-path'. Run the following command:
%s/Isabelle/bin/isabelle install /usr/local/bin" isabelle-bundle)))))

(lunaryorn-after proof-useropts
  (setq proof-three-window-enable nil   ; More predictable window management
        ;; Automatically process the script up to point when inserting a
        ;; terminator.  Really handy in Coq.
        proof-electric-terminator-enable t))

;; Proof General has a rather strange way of creating this variable
(defvar coq-one-command-per-line)
(setq coq-one-command-per-line nil)

(lunaryorn-after proof-script
  (add-hook 'proof-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

(lunaryorn-after isar
  ;; Don't highlight overlong lines in Isar, since Unicode Tokens conceal the
  ;; true line length
  (add-hook 'isar-mode-hook #'lunaryorn-whitespace-style-no-long-lines 'append))


;;; Special modes
(auto-image-file-mode)                  ; Visit images as images


;;; Documentation

(lunaryorn-after info
  ;; Fix the stupid `Info-quoted' face.  Courier is an abysmal face, so go back
  ;; to the default face.
  (set-face-attribute 'Info-quoted nil :family 'unspecified
                      :inherit font-lock-constant-face))


;;; General version control

(lunaryorn-after vc-hooks
  ;; Always follow symlinks to files in VCS repos
  (setq vc-follow-symlinks t))

;; Highlight VCS diffs
;; Highlight changes to the current file in the fringe
(global-diff-hl-mode)
;; Highlight changed files in the fringe of Dired
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
;; Fall back to the display margin, if the fringe is unavailable
(unless (display-graphic-p)
  (diff-hl-margin-mode))


;;; Git support

;; The one and only Git frontend
(lunaryorn-after magit
  ;; Shut up, Magit!
  (setq magit-save-some-buffers 'dontask
        magit-stage-all-confirm nil
        magit-unstage-all-confirm nil
        ;; Except when you ask something useful…
        magit-set-upstream-on-push t
        ;; Use IDO for completion
        magit-completing-read-function #'magit-ido-completing-read)

  ;; Auto-revert files after Magit operations
  (magit-auto-revert-mode)
  (setq magit-auto-revert-mode-lighter ""))


;;; Tools and utilities

;; Powerful file and code search
(lunaryorn-after ag
  (setq ag-reuse-buffers t              ; Don't spam buffer list with ag buffers
        ag-highlight-search t           ; A little fanciness
        ;; Use Projectile to find the project root
        ag-project-root-function (lambda (d) (let ((default-directory d))
                                               (projectile-project-root)))))

(defvar lunaryorn-ag-project-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'ag-project-regexp)
    (define-key map (kbd "A") #'ag-project)
    (define-key map (kbd "d") #'ag-project-dired-regexp)
    (define-key map (kbd "D") #'ag-project-dired)
    (define-key map (kbd "f") #'ag-project-files)
    ;; For symmetry with `lunaryorn-ag-map'
    (define-key map (kbd "k") #'ag-kill-other-buffers)
    (define-key map (kbd "K") #'ag-kill-buffers)
    map)
  "Keymap for Ag's project commands.")

;; Project interaction
(lunaryorn-after projectile
  (setq projectile-completion-system 'ido
        projectile-find-dir-includes-top-level t
        projectile-mode-line '(:propertize
                               (:eval (concat " " (projectile-project-name)))
                               face font-lock-constant-face))

  ;; Replace Ack with Ag in Projectile commander
  (def-projectile-commander-method ?a
    "Find ag on project."
    (call-interactively 'projectile-ag))

  ;; Use our custom Ag bindings in Projectile.  We use functions from ag here,
  ;; because they are way more powerful
  (let ((prefix-map (lookup-key projectile-mode-map projectile-keymap-prefix)))
    (define-key prefix-map "a" lunaryorn-ag-project-map))
  (define-key projectile-mode-map [remap projectile-ag] nil))
(projectile-global-mode)

;; Quickly switch to IELM
(defun lunaryorn-switch-to-ielm ()
  "Switch to an ielm window.

Create a new ielm process if required."
  (interactive)
  (pop-to-buffer (get-buffer-create "*ielm*"))
  (ielm))

;; Google from Emacs, under C-c /
(google-this-mode)

;; Insert date and time
(defun lunaryorn-insert-current-date (iso)
  "Insert the current date at point.

When ISO is non-nil, insert the date in ISO 8601 format.
Otherwise insert the date as Mar 04, 2014."
  (interactive "P")
  (insert (format-time-string (if iso "%F" "%b %d, %Y"))))

(lunaryorn-after paradox
  ;; Don't ask for a token, please
  (setq paradox-github-token t))


;;; Calendar
(lunaryorn-after calendar
  ;; In Europe we start on Monday
  (setq calendar-week-start-day 1))


;;; E-Mail

;; Settings for sending mail via GMail
(lunaryorn-after sendmail
  (setq send-mail-function 'smtpmail-send-it))

(lunaryorn-after message
  (setq message-send-mail-function 'smtpmail-send-it
        message-kill-buffer-on-exit t)) ; Don't keep message buffers around

(lunaryorn-after smtpmail
  (setq smtpmail-smtp-server "vega.uberspace.de"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls
        smtpmail-smtp-user user-login-name))


;;; IRC

(lunaryorn-after erc
  ;; Default server and nick
  (setq erc-server "chat.freenode.net"
        erc-port 7000
        erc-nick "lunaryorn"
        erc-nick-uniquifier "_"
        ;; Never open unencrypted ERC connections
        erc-server-connect-function 'erc-open-tls-stream)

  ;; Spell-check ERC buffers
  (add-to-list 'erc-modules 'spelling)
  (erc-update-modules))

(lunaryorn-after erc-join
  ;; Standard channels on Freenode
  (setq erc-autojoin-channels-alist '(("\\.freenode\\.net" . ("#emacs")))))

(lunaryorn-after erc-track
  ;; Switch to newest buffer by default, and don't ask before rebinding the keys
  (setq erc-track-switch-direction 'newest
        erc-track-enable-keybindings t))

(lunaryorn-after rcirc
  (setq rcirc-default-full-name (format "%s (http://www.lunaryorn.com)"
                                        user-full-name)
        rcirc-default-nick "lunaryorn"
        rcirc-time-format "%Y-%m-%d %H:%M "
        rcirc-server-alist
        '(("chat.freenode.not" :port 7000 :user-name "lunaryorn"
           :encryption tls :channels ("#emacs" "#haskell" "#hakyll" "#zsh"))))

  (add-hook 'rcirc-mode-hook #'flyspell-mode)

  (rcirc-track-minor-mode))


;;; Key bindings

;; Kill some useless bindings
(when (display-graphic-p)
  ;; `suspend-frame' is entirely useless in graphic displays
  (global-set-key (kbd "C-z") nil)
  (global-set-key (kbd "C-x C-z") nil))

;; Improve standard bindings
(global-set-key [remap execute-extended-command] #'smex)
(global-set-key [remap list-buffers] #'ibuffer)
(global-set-key [remap kill-whole-line] #'lunaryorn-smart-kill-whole-line)
(global-set-key [remap move-beginning-of-line]
                #'lunaryorn-back-to-indentation-or-beginning-of-line)
(global-set-key [remap dabbrev-expand] #'hippie-expand)
(global-set-key [remap isearch-forward] #'isearch-forward-regexp)
(global-set-key [remap isearch-backward] #'isearch-backward-regexp)
(global-set-key [remap just-one-space] #'cycle-spacing)
;;; Killing and marking on steroids
(global-set-key [remap kill-ring-save] #'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)
;; Complement standard bindings (the comments indicate the related bindings)
(global-set-key (kbd "M-X") #'smex-major-mode-commands)                  ; M-x
(global-set-key (kbd "C-<backspace>") #'lunaryorn-smart-backward-kill-line) ; C-S-backspace
(global-set-key (kbd "C-S-j") #'lunaryorn-smart-open-line)                  ; C-j
(global-set-key (kbd "M-Z") #'zap-up-to-char)                            ; M-z
(global-set-key (kbd "C-h A") #'apropos)                                 ; C-h a
(global-set-key (kbd "C-x p") #'proced)                                  ; C-x p
;; Find definition sources fast
(find-function-setup-keys)

;; Key bindings for extension packages
(global-set-key (kbd "C-=") #'er/expand-region)

;; Personal keymaps
(defvar lunaryorn-ag-map nil
  "Keymap for Ag.")

(define-prefix-command 'lunaryorn-ag 'lunaryorn-ag-map)
(let ((map lunaryorn-ag-map))
  (define-key map (kbd "a") #'ag-regexp)
  (define-key map (kbd "A") #'ag)
  (define-key map (kbd "d") #'ag-dired-regexp)
  (define-key map (kbd "D") #'ag-dired)
  (define-key map (kbd "f") #'ag-files)
  (define-key map (kbd "k") #'ag-kill-other-buffers)
  (define-key map (kbd "K") #'ag-kill-buffers)
  map)

(defvar lunaryorn-align-map nil
  "Keymap for aligning text.")

(define-prefix-command 'lunaryorn-align 'lunaryorn-align-map)
(let ((map lunaryorn-align-map))
  (define-key map (kbd "a") #'align)
  (define-key map (kbd "c") #'align-current)
  (define-key map (kbd "r") #'align-regexp))

(defvar lunaryorn-files-map nil
  "Keymap for file operations.")

(define-prefix-command 'lunaryorn-file 'lunaryorn-files-map)
(let ((map lunaryorn-files-map))
  (define-key map (kbd "D") #'lunaryorn-delete-file-and-buffer)
  (define-key map (kbd "i") #'lunaryorn-find-user-init-file-other-window)
  (define-key map (kbd "L") #'add-dir-local-variable)
  (define-key map (kbd "l") #'add-file-local-variable)
  (define-key map (kbd "o") #'lunaryorn-launch-dwim)
  (define-key map (kbd "p") #'add-file-local-variable-prop-line)
  (define-key map (kbd "R") #'lunaryorn-rename-file-and-buffer)
  (define-key map (kbd "r") #'lunaryorn-ido-find-recentf)
  (define-key map (kbd "w") #'lunaryorn-copy-filename-as-kill)
  map)

(defvar lunaryorn-list-map nil
  "Keymap to list things.")

(define-prefix-command 'lunaryorn-list 'lunaryorn-list-map)
(let ((map lunaryorn-list-map))
  (define-key map (kbd "b") #'list-bookmarks)
  (define-key map (kbd "e") #'list-flycheck-errors)
  (define-key map (kbd "p") #'paradox-list-packages)
  (define-key map (kbd "P") #'package-list-packages-no-fetch)
  (define-key map (kbd "r") #'list-register)
  (define-key map (kbd "t") #'list-tags)
  map)

(defvar lunaryorn-multiple-cursors-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'mc/mark-more-like-this-extended)
    (define-key map (kbd "h") #'mc/mark-all-like-this-dwim)
    (define-key map (kbd "l") #'mc/edit-lines)
    (define-key map (kbd "n") #'mc/mark-next-like-this)
    (define-key map (kbd "p") #'mc/mark-previous-like-this)
    (define-key map (kbd "r") #'vr/mc-mark)
    (define-key map (kbd "C-a") #'mc/edit-beginnings-of-lines)
    (define-key map (kbd "C-e") #'mc/edit-ends-of-lines)
    (define-key map (kbd "C-s") #'mc/mark-all-in-region)
    map))

(defvar lunaryorn-symbols-map nil
  "Keymap for symbol operations.")

(define-prefix-command 'lunaryorn-symbol 'lunaryorn-symbols-map)
(let ((map lunaryorn-symbols-map))
  (define-key map (kbd "o") #'highlight-symbol-occur)
  (define-key map (kbd "%") #'highlight-symbol-query-replace)
  (define-key map (kbd "n") #'highlight-symbol-next-in-defun)
  (define-key map (kbd "p") #'highlight-symbol-prev-in-defun)
  map)

(defvar lunaryorn-toggle-map nil
  "Keymap to toggle buffer local settings.")

(define-prefix-command 'lunaryorn-toggle 'lunaryorn-toggle-map)
(let ((map lunaryorn-toggle-map))
  (define-key map (kbd "d") #'toggle-debug-on-error) ; Debugging
  (define-key map (kbd "f") #'flycheck-mode)     ; Syntax checking
  (define-key map (kbd "l") #'nlinum-mode)       ; Line numbers in margin
  (define-key map (kbd "s") #'flyspell-mode)     ; Spell checking
  (define-key map (kbd "w") #'whitespace-mode)   ; Whitespace highlighting…
  (define-key map (kbd "W") #'whitespace-cleanup-mode) ; …and cleanup
  map)

(defvar lunaryorn-utilities-map nil
  "Keymap for various utilities.")

(define-prefix-command 'lunaryorn-utility 'lunaryorn-utilities-map)
(let ((map lunaryorn-utilities-map))
  (define-key map (kbd "z") #'lunaryorn-switch-to-ielm)
  (define-key map (kbd "d") #'lunaryorn-insert-current-date)
  map)

;; User key bindings in the C-c space.
(global-set-key (kbd "C-c SPC") #'ace-jump-mode)
(global-set-key (kbd "C-c a") #'lunaryorn-ag)
(global-set-key (kbd "C-c A") #'lunaryorn-align)
(global-set-key (kbd "C-c B") #'browse-url)
(global-set-key (kbd "C-c c") #'compile)
(global-set-key (kbd "C-c C") #'recompile)
(global-set-key (kbd "C-c f") #'lunaryorn-file)
(global-set-key (kbd "C-c g") #'magit-status)
(global-set-key (kbd "C-c i") #'imenu-anywhere)
(global-set-key (kbd "C-c j") #'ace-jump-mode)
(global-set-key (kbd "C-c J") #'ace-jump-mode-pop-mark)
(global-set-key (kbd "C-c l") #'lunaryorn-list)
(global-set-key (kbd "C-C M") #'recompile)
(global-set-key (kbd "C-c m") lunaryorn-multiple-cursors-map)
(global-set-key (kbd "C-c o") #'occur)
(global-set-key (kbd "C-c r") #'vr/query-replace)
(global-set-key (kbd "C-c R") #'vr/replace)
(global-set-key (kbd "C-c s") #'lunaryorn-symbol)
(global-set-key (kbd "C-c t") #'lunaryorn-toggle)
(global-set-key (kbd "C-c u") #'lunaryorn-utility)
(global-set-key (kbd "C-c y") #'browse-kill-ring)

(lunaryorn-after lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c e") #'macrostep-expand)
  (define-key emacs-lisp-mode-map (kbd "C-c f c") #'lunaryorn-find-cask-file)

  (define-key lisp-interaction-mode-map (kbd "C-c e") #'macrostep-expand)
  (define-key lisp-interaction-mode-map (kbd "C-j") #'eval-print-last-sexp))

(lunaryorn-after markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-s C")
    #'markdown-insert-gfm-code-block)
  (define-key markdown-mode-map (kbd "C-c C-s P")
    #'markdown-insert-gfm-code-block))

(lunaryorn-after haskell-mode
  (let ((map haskell-mode-map))
    (define-key map (kbd "C-c h") #'haskell-hayoo)
    (define-key map (kbd "C-c H") #'haskell-hoogle)
    (define-key map (kbd "C-c d") #'haskell-describe)
    (define-key map (kbd "C-c f c") #'haskell-cabal-visit-file)
    ;; Some convenience bindings
    (define-key map (kbd "C-c I") #'haskell-navigate-imports)
    (define-key map (kbd "M-.") #'haskell-mode-jump-to-def-or-tag)))

(lunaryorn-after tuareg
  ;; Please, Tuareg, don't kill my imenu
  (define-key tuareg-mode-map [?\C-c ?i] nil))

(lunaryorn-after merlin
  (define-key merlin-mode-map (kbd "C-c t e") #'merlin-toggle-view-errors))

(lunaryorn-after shm
  (define-key shm-map (kbd "C-c C-s") 'shm/case-split))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
