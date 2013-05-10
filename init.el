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

;; Your personal Emacs configuration.  Load Stante Pede, and choose your modules
;; wisely.


;;; Code:


;;;; Guard against evil Emacs 2
(when (or (< emacs-major-version 24)
          (and (= emacs-major-version 24) (< emacs-minor-version 1)))
  (error "Stante Pede needs at least GNU Emacs 24.1, but this is Emacs %s.
Please install GNU Emacs 24.1 to use Stante Pede"
         emacs-version))


;;;; Basic definitions
(defmacro after (file &rest forms)
  "Evaluate FORMS after FILE is loaded.

FILE may be a named feature, see `eval-after-load'."
  (declare (indent 1))
  `(eval-after-load ,file
     '(progn ,@forms)))


;;;; Stante Pede libraries and autoloads
(defconst stante-lib-dir (locate-user-emacs-file "lib")
  "Directory of Stante Pede libraries.")
(add-to-list 'load-path stante-lib-dir)
(load (expand-file-name "stante-autoloads" stante-lib-dir) nil)


;;;; Custom file
(defconst stante-custom-file (locate-user-emacs-file "custom.el")
  "Location of the Customize file.")

(eval-after-load 'cus-edit
  '(setq custom-file stante-custom-file))

(load stante-custom-file t t)


;;;; Load and initialize packages
(require 'package)
(package-initialize)
(require 'carton)
(carton-setup user-emacs-directory)

;; Custom vendor extensions
(defconst stante-vendor-dir (locate-user-emacs-file "vendor")
  "Directory for embedded 3rd party extensions.")


;;;; Requires
(require 'dash)

(eval-when-compile
  (require 'ido)
  (require 'ediff-wind)
  (require 'smex)
  (require 'page-break-lines))


;;;; User interface settings

;; Color theme!
(load-theme 'solarized-light t)
;; (load-theme 'solarized-dark t)
;; (load-theme 'zenburn t)

(when (display-graphic-p)
  ;; Fix `exec-path' and $PATH for graphical Emacs by letting a shell output
  ;; the `$PATH'.
  (exec-path-from-shell-initialize))

;; Disable toolbar and menu bar (except on OS X where the menubar is present
;; anyway)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(unless (eq system-type 'darwin)
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)))

;; Disable blinking cursor
(blink-cursor-mode -1)

;; Nice page breaks
(global-page-break-lines-mode)
(after 'page-break-lines
  (diminish 'page-break-lines-mode)

    ;; Fix the font of lines
  (set-fontset-font "fontset-default"
                    (cons page-break-lines-char page-break-lines-char)
                    (face-attribute 'default :family)))

;; Disable alarm beeps
(setq ring-bell-function 'ignore)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Do not needlessly signal errors
(setq scroll-error-top-bottom t)

;; Smoother scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1))

;; Improve mode line
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Unify Yes/No questions
(fset 'yes-or-no-p 'y-or-n-p)

;; Show file name or buffer name in frame title
(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

;; De-duplicate buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t       ; Re-uniquify after killing buffers
      uniquify-ignore-buffers-re "^\\*")   ; Ignore special buffers


;; Improve completion for file and buffer names
(ido-mode t)
(setq ido-enable-flex-matching t    ; Match characters if string doesn't match
      ido-create-new-buffer 'always ; Create a new buffer if nothing matches
      ido-use-filename-at-point 'guess
      ido-default-file-method 'selected-window)

;; Improve minibuffer completion
(icomplete-mode)

;; Improved M-x
(after 'smex
  (setq smex-save-file (locate-user-emacs-file ".smex-items")))

;; Move between windows with Shift + Arrows
(windmove-default-keybindings)

;; Window management reloaded
(winner-mode)

;; Nice default font, download from http://www.google.com/fonts/
(set-face-attribute 'default nil :family "Inconsolata" :height 150)

;; Reuse current frame for EDiff
(after 'ediff-wind
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; Save and restore the frame size and parameters
(defvar stante-save-frame-parameters-file
  (locate-user-emacs-file ".frame-parameters" )
  "File in which to storce frame parameters on exit.")

(defconst stante-frame-parameters-to-save
  '(left top width height maximized fullscreen)
  "Frame parameters to save and restore for the initial frame.")

(defun stante-restore-frame-parameters ()
  "Restore the frame parameters of the initial frame."
  (condition-case nil
      (-when-let* ((read-params
                    (with-temp-buffer
                      (insert-file-contents stante-save-frame-parameters-file)
                      (goto-char (point-min))
                      (read (current-buffer))))
                   (allowed-params
                    (--filter (memq (car it) stante-frame-parameters-to-save)
                              read-params)))
        (setq initial-frame-alist
              (append (--filter (assq (car it) allowed-params) initial-frame-alist)
                      allowed-params nil)))
    (error nil)))

(defun stante-save-frame-parameters ()
  "Save frame parameters of the selected frame.

Save selected parameters (see `stante-frame-parameters-to-save')
to `stante-save-frame-parameters-file'."
  (condition-case nil
      (let ((params (--filter (memq (car it) stante-frame-parameters-to-save)
                              (frame-parameters))))
        (when (and params (display-graphic-p)) ; GUI frames only!
          (with-temp-buffer
            (prin1 params (current-buffer))
            (terpri (current-buffer))
            (write-region (point-min) (point-max)
                          stante-save-frame-parameters-file
                          nil 0))       ; 0 inhibits the "write file" message
          t))
    (file-error nil)))

(unless noninteractive
  (add-hook 'after-init-hook 'stante-restore-frame-parameters)
  (add-hook 'kill-emacs-hook 'stante-save-frame-parameters))

(defun stante-switch-to-previous-buffer ()
  "Switch to the previous buffer.

Repeated invocations toggle between the two most recently used
buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) :visible-ok)))


;;;; OS X support
(when (eq system-type 'darwin)
  ;; Find GNU Coreutils (mostly for "ls --dired").
  (let ((gls (executable-find "gls")))
    (if gls
        (setq insert-directory-program gls)
      (message "GNU Coreutils not found.  Install coreutils \
with homebrew, or report an issue with M-x stante-report-issue.")))

  ;; Ignore OS X metadata files in IDO
  (after 'ido
    (add-to-list 'ido-ignore-files "\\`\\.DS_Store\\'")))

(after 'ns-win
  ;; Setup modifier maps for OS X
  (setq mac-option-modifier 'meta
        mac-command-modifier 'meta
        mac-function-modifier 'hyper
        mac-right-option-modifier 'none
        mac-right-command-modifier 'super)

  ;; Do not popup new frames
  (setq ns-pop-up-frames nil))


;;;; Basic editor settings
;; Move backup files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup"))))

;; Sane coding system choice
(prefer-coding-system 'utf-8)

;; Advanced undo system
(global-undo-tree-mode)
(after 'undo-tree
  (diminish 'undo-tree-mode))

;; Automatically revert buffers from changed files
(global-auto-revert-mode 1)

;; Delete selection when entering new text
(delete-selection-mode)

;; View readonly files
(setq view-read-only t)

;; Preserve clipboard text before killing
;; This would be a really nice addition, but Emacs has the stupid habit
;; of constantly signalling errors if the paste content is not supported or
;; empty. Obviously this breaks killing completely.  Really great thanks to you,
;; whoever crazy mind got that silly idea…
;; (setq save-interprogram-paste-before-kill t)

;; No tabs for indentation
(setq-default indent-tabs-mode nil
              tab-width 8)

;; Drag stuff around
(drag-stuff-global-mode)
(after 'drag-stuff
  (setq drag-stuff-modifier '(meta shift))
  (diminish 'drag-stuff-mode))

;; Configure filling
(setq-default fill-column 80)
(after 'whitespace
  (setq whitespace-line-column nil))
(--each '(prog-mode-hook text-mode-hook)
  (add-hook it 'fci-mode))

;; Configure wrapping
(add-hook 'text-mode-hook 'adaptive-wrap-prefix-mode)

;; Power up parenthesis
(smartparens-global-mode)
(show-smartparens-global-mode)
(after 'smartparens
  (require 'smartparens-config)

  ;; Use Paredit-like keybindings.  The Smartparens bindings are too obtrusive,
  ;; shadow otherwise useful bindings (e.g. M-<backspace>), and use the arrow
  ;; keys too much
  (sp-use-paredit-bindings)

  (diminish 'smartparens-mode))

;; Highlights
(global-hl-line-mode 1)
(require 'volatile-highlights)          ; Volatile Highlights doesn't autoload
(volatile-highlights-mode t)
(after 'volatile-highlights (diminish 'volatile-highlights-mode))

(after 'whitespace
  (setq whitespace-style '(face tabs empty trailing lines-tail))
  (diminish 'whitespace-mode))

(setq indicate-empty-lines t)

;; Cleanup stale buffers
(require 'midnight)

;; Narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Remember minibuffer history
(after 'savehist
  (setq savehist-save-minibuffer-history t
        ;; Save every three minutes (the default five minutes is a bit long)
        savehist-autosave-interval 180))
(savehist-mode t)

;; Recent files
(after 'recentf
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15))
(recentf-mode t)

;; Remember locations in files
(after 'saveplace
  (setq-default save-place t))
(require 'saveplace)

;; Configure bookmarks
(after 'bookmark
  ;; Save on every modification
  (setq bookmark-save-flag 1))

;; Completion
(setq completion-cycle-threshold 5)     ; Cycle with less than 5 candidates

;; Expansion functions
(after 'hippie-exp
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

;; Completion with Company
(global-company-mode)

(after 'company
  (diminish 'company-mode)

  ;; Make completion a little less aggressive
  (setq company-idle-delay 1.0
        company-begin-commands '(self-insert-command)
        ;; Make completion a bit more fancy
        company-show-numbers t))

;; Snippets with Yasnippet
(yas-global-mode 1)
(after 'yasnippet
  (diminish 'yas-minor-mode))

;; Bring up Emacs server
(require 'server)
(unless (server-running-p) (server-start))

;; Flymake reloaded :)
(global-flycheck-mode)

;; Update copyright lines automatically
(add-hook 'find-file-hook 'copyright-update)

;; Some missing autoloads
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")


;;;; Spell checking
(unless (executable-find "aspell")
  (message "Aspell not found.  Spell checking may not be available!"))

;; Choose English as default languages, because programming is mostly done in
;; this languages, and don't ask to save the private dictionary.
(after 'ispell
  (setq ispell-dictionary "en"
        ispell-silently-savep t))

(after 'flyspell
  ;; Get M-Tab and C-M-i back for `completion-at-point'
  (define-key flyspell-mode-map "\M-\t" nil)
  ;; and prevent it from ever taking it again
  (setq flyspell-use-meta-tab nil))

(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(after 'guru-mode (diminish 'guru-mode))


;;;; Tools and utilities
;; Convenience aliases for Ack
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; Search with Google
(google-this-mode)
(after 'google-this
  (diminish 'google-this-mode))

;; Project management
(projectile-global-mode)
(after 'projectile
  (diminish 'projectile-mode))


;;;; Git support
(after 'magit
  ;; Do not ask before saving buffers on `magit-status', but ask whether to set
  ;; upstream branch when pushing a branch without upstream.  Also exclude
  ;; remote name from names of tracking branches
  (setq magit-save-some-buffers 'dontask
        magit-set-upstream-on-push t
        magit-default-tracking-name-function 'magit-default-tracking-name-branch-only))

;; Open newly created Gists in browser.
(after 'gist
  (setq gist-view-gist t))

;; Indicate Git changes
(global-git-gutter-mode)
(after 'git-gutter
  (diminish 'git-gutter-mode)

  ;; Show Git Gutter on the right fringe, the left fringe is for Flycheck errors
  (require 'git-gutter-fringe))


;;;; Plain text editing
(after "text-mode"
  (--each '(turn-on-auto-fill guru-mode stante-text-whitespace-mode)
    (add-hook 'text-mode-hook it)))


;;;; Markdown editing
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(after 'markdown-mode
  (stante-find-markdown-processor)

  ;; Disable filling in Gfm mode, because line breaks have a meaning in Gfm
  (--each '(turn-off-auto-fill turn-off-fci-mode)
    (add-hook 'gfm-mode-hook it))

  (after 'smartparens
    (sp-with-modes '(markdown-mode gfm-mode)
      (sp-local-pair "*" "*")
      (sp-local-tag "c" "```scheme" "```"))))


;;;; TeX/LaTeX/Texinfo editing
(when (eq system-type 'darwin)

  (require 'stante-os-x)
  (when (stante-homebrew-installed-p "auctex")
    (let ((homebrew-prefix (stante-homebrew-prefix)))
      (add-to-list 'load-path (expand-file-name "share/emacs/site-lisp"
                                                homebrew-prefix)))))

(unless (require 'tex-site nil t)
  (message "AUCTeX not installed.  LaTeX/Texinfo editing is limited!"))
(unless (require 'preview-latex nil t))

;; Handle .latex files with AUCTeX, too.
(add-to-list 'auto-mode-alist '("\\.[lL]a[tT]e[xX]\\'" . latex-mode))

(after 'tex-site
  (setq TeX-auto-save t        ; Autosave documents
        TeX-parse-self t       ; Parse documents
        TeX-save-query nil     ; Don't ask before saving
        TeX-clean-confirm nil  ; Don't ask before cleaning
        ;; Enable forward and inverse search with SyncTeX
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex)
  (setq-default TeX-master nil ; Ask for master document
                ;; Use a modern LaTeX engine to build PDFs
                TeX-engine 'luatex
                TeX-PDF-mode t)

  ;; Setup sub modes
  (--each '(reftex-mode LaTeX-math-mode) (add-hook 'LaTeX-mode-hook it))

  ;; Enable Smartparens support
  (after 'smartparens
    (require 'smartparens-latex)))

;; Provide latexmk support.
(after 'tex
  (unless (boundp 'TeX-command-latexmk) ; Just in case this ever gets upstreamed
    (defvar TeX-command-latexmk "latexmk"
      "The name of the latexmk command.")

    ;; Declare the latexmk command
    (unless (assoc TeX-command-latexmk TeX-command-list)
      (add-to-list 'TeX-command-list
                   `(,TeX-command-latexmk "latexmk" TeX-run-command t t
                                          :Help "Run latexmk"))))

  ;; Replace lacheck with chktex for "Check" command
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s"))

(after 'latex
  ;; Clean latexmk files
  (--each '("\\.fdb_latexmk" "\\.fls")
    (add-to-list 'LaTeX-clean-intermediate-suffixes it)))

;; Select best viewing programs
(after 'tex (stante-TeX-select-view-programs))

;; Configure Texinfo editing with AUCTeX
(after 'tex-info
  (add-hook 'Texinfo-mode-hook 'reftex-mode)  ; Enable RefTeX

  ;; Bind some convenient commands from the built-in Texinfo mode.
  (define-key Texinfo-mode-map (kbd "C-c c") 'makeinfo-buffer))


;;;; BibTeX editing and RefTeX setup
(after 'bib-cite
  (setq bib-cite-use-reftex-view-crossref t))

;; Configure bibtex editing to use biblatex by default
(after 'bibtex
  (bibtex-set-dialect 'biblatex)
  ;; Configure exhaustive cleanup of bibtex entries
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

;; BibTeX manager
(after 'ebib
  (setq ebib-autogenerate-keys t))

;; Configure RefTeX
(after 'reftex
  (setq reftex-plug-into-AUCTeX t
        ;; Recommended optimizations
        reftex-enable-partial-scans t
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t)

  (unless (assq 'biblatex reftex-cite-format-builtin)
    ;; Add biblatex support if not already builtin
    (add-to-list 'reftex-cite-format-builtin
                 '(biblatex "The biblatex package"
                            ((?\C-m . "\\cite[]{%l}")
                             (?t . "\\textcite{%l}")
                             (?a . "\\autocite[]{%l}")
                             (?p . "\\parencite{%l}")
                             (?f . "\\footcite[][]{%l}")
                             (?F . "\\fullcite[]{%l}")
                             (?x . "[]{%l}")
                             (?X . "{%l}")))))
  (setq reftex-cite-format 'biblatex) ; Use Biblatex as default citation style
  ;; Make RefTeX recognize biblatex bibliographies
  (add-to-list 'reftex-bibliography-commands "addbibresource"))


;;;; General programming settings
;; Default semantic submodes
(setq semantic-default-submodes
      '(global-semanticdb-minor-mode ;; Cache database
        global-semantic-idle-scheduler-mode ;; Re-parse when idle
        global-semantic-idle-summary-mode ;; Show tab summary when idle
        global-semantic-stickyfunc-mode ;; Show current tag at top of buffer
        ))

(semantic-mode 1)

(after 'highlight-symbol
  ;; Highlight the symbol under point after short delay, and highlight the
  ;; symbol immediately after symbol navigation
  (setq highlight-symbol-idle-delay 0.4
        highlight-symbol-on-navigation-p t)

  (diminish 'highlight-symbol-mode))

(after 'simple ; prog-mode is contained in simple.el
  ;; A set of reasonable programmming modes
  (--each '(stante-auto-fill-comments-mode ; Fill in comments
            stante-prog-whitespace-mode    ; Highlight and cleanup whitespace
            guru-mode                      ; Disable non-emacsy bindings
            highlight-symbol-mode          ; Highlight symbol under point
            )
    (add-hook 'prog-mode-hook it)))


;;;; Emacs Lisp programming
(after 'smartparens
  ;; Wrap with parenthesis on M-( for compatibility with paredit
  (sp-with-modes sp--lisp-modes
    (sp-local-pair "(" nil :bind "M-(")))

(defun stante-font-lock-add-ert-keywords ()
  "Add font lock keywords supporting ERT tests."
  (font-lock-add-keywords
   nil
   '(("(\\(\\<ert-deftest\\)\\>\\s *\\(\\sw+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t)))))

(after 'lisp-mode
  (--each '(emacs-lisp-mode-hook ielm-mode-hook)
    (add-hook it 'turn-on-eldoc-mode)
    (add-hook it 'rainbow-delimiters-mode))

  (--each '(checkdoc-minor-mode
            stante-emacs-lisp-clean-byte-code-mode
            stante-font-lock-add-ert-keywords)
    (add-hook 'emacs-lisp-mode-hook it))

  ;; Explicitly enable Smartparens in IELM.  The global mode won't do it,
  ;; because IELM is a special mode, in which Global Smartparens Mode refuses to
  ;; enable Smartparens mode
  (add-hook 'ielm-mode-hook #'smartparens-mode)

  ;; Indent ERT tests like functions
  (put 'ert-deftest 'lisp-indent-function 'defun)

  ;; Recognize Emacs scripts
  (add-to-list 'interpreter-mode-alist '("emacs" . emacs-lisp-mode))

  ;; Consider Carton files as Emacs Lisp files
  (add-to-list 'auto-mode-alist '("Carton\\'" . emacs-lisp-mode)))

;; De-clutter mode line
(after 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
(after 'eldoc (diminish 'eldoc-mode))
(after 'checkdoc (diminish 'checkdoc-minor-mode))
(after 'rainbow-delimiters (diminish 'rainbow-delimiters-mode))


;;;; Python programming
(defun stante-python-filling ()
  "Configure filling for Python."
  ;; PEP 8 recommends a maximum of 79 characters
  (setq fill-column 79))

;; Find the best checker
(after 'python
  (--each '(stante-python-filling subword-mode)
    (add-hook 'python-mode-hook it))

  (setq python-check-command "flake8")

  ;; Default to Python 3 if available
  (let ((python3 (executable-find "python3")))
    (when python3
      (setq python-shell-interpreter python3))))

(after 'expand-region
  ;; Tell expand-region about the Python mode we're using
  (setq expand-region-guess-python-mode nil
        expand-region-preferred-python-mode 'fgallina-python))


;;;; Shell scripting
(after 'sh-script
  ;; Standard indentation styles
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

  (defun stante-sh-set-default-style ()
    "Set a standard indentation style."
    (sh-load-style "zsh"))

  (add-hook 'sh-mode-hook #'stante-sh-set-default-style))

;; Also consider .zsh files as `sh-mode' files.
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))


;;;; Various other programming languages
(after 'coffee-mode
  ;; CoffeeScript should use two spaces for indentation
  (setq coffee-tab-width 2))

(after 'haskell-mode
  ;; Some standard Haskell settings
  (--each '(subword-mode
            turn-on-haskell-indentation
            turn-on-haskell-doc-mode
            turn-on-haskell-decl-scan)
    (add-hook 'haskell-mode-hook it)))

(after 'scss-mode
  ;; Do not compile SCSS after saving
  (setq scss-compile-at-save nil))

(after 'nxml-mode
  ;; Complete closing XML tags and insert XML declarations in new XML files
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t))


;;;; Proof General
(when (eq system-type 'darwin)
  (require 'stante-os-x)

  (defconst stante-isabelle-bin-dir
    (-when-let (bundle-directory (stante-path-of-bundle "de.tum.in.isabelle"))
      (expand-file-name "Contents/Resources/Isabelle/bin" bundle-directory)))

  (setq isa-isabelle-command
        (expand-file-name "isabelle" stante-isabelle-bin-dir))

  (after 'isabelle-system
    (setq isabelle-program-name-override
          (expand-file-name "isabelle-process" stante-isabelle-bin-dir))))

(load (expand-file-name "ProofGeneral/generic/proof-site.el" stante-vendor-dir))

(after 'proof-useropts
  ;; Do not spam the frame with windows when executing a buffer
  (setq proof-three-window-enable nil))

(after 'isar-syntax
  (set-face-attribute 'isabelle-string-face nil
                      :foreground nil :background nil
                      :inherit 'font-lock-string-face)
  (set-face-attribute 'isabelle-quote-face nil
                      :foreground nil :background nil
                      :inherit 'font-lock-string-face))


;;;; Org mode
(defun stante-org-disable-incompatible-modes ()
  "Disable minor modes incompatible with Org mode.

This includes:

- Guru Mode
- Drag Stuff Mode"
  (guru-mode -1)
  (drag-stuff-mode -1))

(after 'org
  ;; Make windmove work in org-mode
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  (add-hook 'org-mode-hook #'stante-org-disable-incompatible-modes)

  ;; Use IDO for switching between org buffers
  (setq org-completion-use-ido t
        org-outline-path-complete-in-steps nil)

  ;; Put the Org directory into the Dropbox
  (setq org-directory (expand-file-name "~/Dropbox/Org")
        org-agenda-files (list org-directory))

  ;; Create the directory for Org files
  (unless (file-directory-p org-directory)
    (make-directory org-directory)))

(after 'org-mobile
  ;; Org mobile synchronization
  (setq org-mobile-directory "~/Dropbox/Org/Mobile"
        org-mobile-inbox-for-pull
        (expand-file-name "from-mobile.org" org-directory))

  (unless (file-directory-p org-mobile-directory)
    (make-directory org-mobile-directory)))


;;;; Global key bindings
;; Do not bind into C-c here, such bindings belong to the next page.  This page
;; is only concerned with global key bindings of Emacs standard keys
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Similar to C-x d
(global-set-key (kbd "C-x p") 'proced)
;; Complementary to C-h a
(global-set-key (kbd "C-h A") 'apropos)
;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; Swap isearch and isearch-regexp
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
;; Smarter line killing and opening
(global-set-key (kbd "C-<backspace>") 'stante-smart-backward-kill-line)
(global-set-key [remap kill-whole-line] 'stante-smart-kill-whole-line)
(global-set-key (kbd "C-S-j") 'stante-smart-open-line)
(global-set-key [(shift return)] 'stante-smart-open-line)
(global-set-key (kbd "M-Z") 'zap-up-to-char)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-=") 'er/expand-region) ; As suggested by documentation
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(global-set-key (kbd "M-<tab>") 'company-complete)


;;;; User keybindings
(defvar stante-ack-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'ack-and-a-half)
    (define-key map (kbd "s") #'ack-and-a-half-same))
  "Keymap for Ack.")

(defvar stante-gist-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'gist-region-or-buffer)
    (define-key map "l" #'gist-list)
    map)
  "Keymap for Gists.")

(defvar stante-multiple-cursors-map
  (let ((map (make-sparse-keymap)))
    (define-key map "l" #'mc/edit-lines)
    (define-key map (kbd "C-a") #'mc/edit-beginnings-of-lines)
    (define-key map (kbd "C-e") #'mc/edit-ends-of-lines)
    (define-key map (kbd "C-s") #'mc/mark-all-in-region)
    (define-key map ">" #'mc/mark-next-like-this)
    (define-key map "<" #'mc/mark-previous-like-this)
    (define-key map "e" #'mc/mark-more-like-this-extended)
    (define-key map "h" #'mc/mark-all-like-this-dwim)
    map)
  "Keymap for Multiple Cursors.")

(defvar stante-file-commands-map
  (let ((map (make-sparse-keymap)))
    (define-key map "r" #'stante-ido-find-recentf)
    (define-key map "o" #'stante-open-with)
    (define-key map "R" #'stante-rename-file-and-buffer)
    (define-key map "D" #'stante-delete-file-and-buffer)
    (define-key map "w" #'stante-copy-filename-as-kill)
    map)
  "Keymap for file functions.")

(defvar stante-symbols-map
  (let ((map (make-sparse-keymap)))
    (define-key map "o" #'highlight-symbol-occur)
    (define-key map "%" #'highlight-symbol-query-replace)
    (define-key map "n" #'highlight-symbol-next-in-defun)
    (define-key map "p" #'highlight-symbol-prev-in-defun)
    (define-key map (kbd "M-n") #'highlight-symbol-next)
    (define-key map (kbd "M-p") #'highlight-symbol-prev)
    map)
  "Keymap to work on symbols.")

;; Key bindings into the C-c user map.
(let ((map mode-specific-map))
  (define-key map "a" stante-ack-map)
  (define-key map "A" 'org-agenda)
  (define-key map "b" 'stante-switch-to-previous-buffer)
  (define-key map "C" 'org-capture)
  (define-key map "f" stante-file-commands-map)
  (define-key map "g" 'magit-status)
  (define-key map "G" stante-gist-map)
  (define-key map "h" 'helm-mini)
  (define-key map "m" stante-multiple-cursors-map)
  (define-key map "o" 'occur)
  (define-key map "s" stante-symbols-map))

;; Mode specific bindings
(after 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c e") #'macrostep-expand)
  (define-key emacs-lisp-mode-map (kbd "C-c z")
    #'stante-emacs-lisp-switch-to-ielm))

;; Local Variables:
;; coding: utf-8
;; End:

;;; init.el ends here
