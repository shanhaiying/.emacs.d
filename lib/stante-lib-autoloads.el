;;; stante-lib-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (stante-TeX-select-view-programs) "stante-lib-TeX-viewers"
;;;;;;  "stante-lib-TeX-viewers.el" (20781 60312 0 0))
;;; Generated autoloads from stante-lib-TeX-viewers.el

(autoload 'stante-TeX-select-view-programs "stante-lib-TeX-viewers" "\
Select the best view programs for the current platform.

\(fn)" nil nil)

;;;***

;;;### (autoloads (stante-smart-open-line stante-smart-kill-whole-line
;;;;;;  stante-smart-backward-kill-line) "stante-lib-editor" "stante-lib-editor.el"
;;;;;;  (20837 25595 0 0))
;;; Generated autoloads from stante-lib-editor.el

(autoload 'stante-smart-backward-kill-line "stante-lib-editor" "\
Kill line backwards and re-indent.

\(fn)" t nil)

(autoload 'stante-smart-kill-whole-line "stante-lib-editor" "\
Kill whole line and move back to indentation.

Kill the whole line with function `kill-whole-line' and then move
`back-to-indentation'.

\(fn &optional ARG)" t nil)

(autoload 'stante-smart-open-line "stante-lib-editor" "\
Insert empty line after the current line.

\(fn)" t nil)

;;;***

;;;### (autoloads (stante-get-file-lines stante-set-file-contents
;;;;;;  stante-get-file-contents) "stante-lib-io" "stante-lib-io.el"
;;;;;;  (20781 60308 0 0))
;;; Generated autoloads from stante-lib-io.el

(autoload 'stante-get-file-contents "stante-lib-io" "\
Return the contents of the file FILENAME.

\(fn FILENAME)" nil nil)

(autoload 'stante-set-file-contents "stante-lib-io" "\
Set the contents of the file FILENAME.

Create the file FILENAME if it does not exist, or completely
overwrite it if it does.

\(fn FILENAME CONTENTS)" nil nil)

(autoload 'stante-get-file-lines "stante-lib-io" "\
Return a list of lines of file FILENAME.

\(fn FILENAME)" nil nil)

;;;***

;;;### (autoloads (after) "stante-lib-load" "stante-lib-load.el"
;;;;;;  (20781 60309 0 0))
;;; Generated autoloads from stante-lib-load.el

(autoload 'after "stante-lib-load" "\
Evaluate FORMS after FILE is loaded.

FILE may be a named feature, see `eval-after-load'.

\(fn FILE &rest FORMS)" nil t)

(put 'after 'lisp-indent-function '1)

;;;***

;;;### (autoloads (stante-byte-recompile stante-update-autoload-file)
;;;;;;  "stante-lib-maintenance" "stante-lib-maintenance.el" (20781
;;;;;;  60310 0 0))
;;; Generated autoloads from stante-lib-maintenance.el

(autoload 'stante-update-autoload-file "stante-lib-maintenance" "\
Update the autoload file of Stante Pede.

\(fn)" t nil)

(autoload 'stante-byte-recompile "stante-lib-maintenance" "\
Byte-compile all modules of Stante pede.

\(fn &optional FORCE)" t nil)

;;;***

;;;### (autoloads (stante-homebrew-installed-p stante-homebrew-prefix
;;;;;;  stante-path-of-bundle) "stante-lib-os-x" "stante-lib-os-x.el"
;;;;;;  (20781 60311 0 0))
;;; Generated autoloads from stante-lib-os-x.el

(autoload 'stante-path-of-bundle "stante-lib-os-x" "\
Get the path of a bundle with ID.

ID is the bundle ID (see `stante-id-of-bundle' as string.  Return
the directory path of the bundle as string.

\(fn ID)" nil nil)

(autoload 'stante-homebrew-prefix "stante-lib-os-x" "\
Get the homebrew prefix for FORMULA.

Without FORMULA, get the homebrew prefix itself.

Return nil, if homebrew is not available, or if the prefix
directory does not exist.

\(fn &optional FORMULA)" nil nil)

(autoload 'stante-homebrew-installed-p "stante-lib-os-x" "\
Determine whether a homebrew FORMULA is installed.

Without FORMULA determine whether Homebrew itself is available.

\(fn &optional FORMULA)" nil nil)

;;;***

;;;### (autoloads (stante-is-windows stante-is-gnome stante-is-kde
;;;;;;  stante-is-os-x) "stante-lib-platform" "stante-lib-platform.el"
;;;;;;  (20781 60311 0 0))
;;; Generated autoloads from stante-lib-platform.el

(autoload 'stante-is-os-x "stante-lib-platform" "\
Return t if running on OS X, or nil otherwise.

\(fn)" nil nil)

(autoload 'stante-is-kde "stante-lib-platform" "\
Return t if running in a KDE session, or nil otherwise.

\(fn)" nil nil)

(autoload 'stante-is-gnome "stante-lib-platform" "\
Return t if running on Gnome, or nil otherwise.

\(fn)" nil nil)

(autoload 'stante-is-windows "stante-lib-platform" "\
Return t if running natively on Windows, or nil otherwise.

Return nil if running in cygwin or under MS-DOS.

\(fn)" nil nil)

;;;***

(provide 'stante-lib-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; stante-lib-autoloads.el ends here
