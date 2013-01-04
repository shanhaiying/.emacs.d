;;; stante-lib-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (stante-TeX-select-view-programs) "stante-lib-TeX-viewers"
;;;;;;  "stante-lib-TeX-viewers.el" (20710 59983))
;;; Generated autoloads from stante-lib-TeX-viewers.el

(autoload 'stante-TeX-select-view-programs "stante-lib-TeX-viewers" "\
Select the best view programs for the current platform.

\(fn)" nil nil)

;;;***

;;;### (autoloads (stante-get-file-lines stante-set-file-contents
;;;;;;  stante-get-file-contents) "stante-lib-io" "stante-lib-io.el"
;;;;;;  (20710 60037))
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
;;;;;;  (20710 60051))
;;; Generated autoloads from stante-lib-load.el

(autoload 'after "stante-lib-load" "\
Evaluate FORMS after FILE is loaded.

FILE may be a named feature, see `eval-after-load'.

\(fn FILE &rest FORMS)" nil (quote macro))

(put 'after 'lisp-indent-function '1)

;;;***

;;;### (autoloads (stante-report-issue stante-byte-recompile stante-update-autoload-file)
;;;;;;  "stante-lib-maintenance" "stante-lib-maintenance.el" (20710
;;;;;;  60309))
;;; Generated autoloads from stante-lib-maintenance.el

(autoload 'stante-update-autoload-file "stante-lib-maintenance" "\
Update the autoload file of Stante Pede.

\(fn)" t nil)

(autoload 'stante-byte-recompile "stante-lib-maintenance" "\
Byte-compile all modules of Stante pede.

\(fn &optional FORCE)" t nil)

(autoload 'stante-report-issue "stante-lib-maintenance" "\
Report an issue with TITLE to Stante Pede.

If called interactively, prompt for TITLE.

Pop up a buffer to edit the issue comment.  If `gfm-mode' is
available, use it in this buffer, otherwise fall back to
`text-mode'.

In this buffer, use C-c C-c s to submit the issue,
and C-c C-c k to cancel the process.

\(fn TITLE)" t nil)

;;;***

;;;### (autoloads (stante-path-of-bundle stante-find-os-x-coreutils)
;;;;;;  "stante-lib-os-x" "stante-lib-os-x.el" (20710 60033))
;;; Generated autoloads from stante-lib-os-x.el

(autoload 'stante-find-os-x-coreutils "stante-lib-os-x" "\
Return the directory containing the unprefixed GNU coreutils on OS X.

If the directory cannot be determined, return nil.

Currently this function only checks for coreutils installed with
homebrew.  In future, more sophisticated logic might be added.

\(fn)" nil nil)

(autoload 'stante-path-of-bundle "stante-lib-os-x" "\
Get the path of a bundle with ID.

ID is the bundle ID (see `stante-id-of-bundle' as string.  Return
the directory path of the bundle as string.

\(fn ID)" nil nil)

;;;***

;;;### (autoloads (package-need) "stante-lib-package" "stante-lib-package.el"
;;;;;;  (20710 59976))
;;; Generated autoloads from stante-lib-package.el

(autoload 'package-need "stante-lib-package" "\
Ensure that the package NAME is available.

If the package is not available, it is automatically installed.

\(fn NAME)" nil nil)

;;;***

;;;### (autoloads (stante-is-windows stante-is-gnome stante-is-kde
;;;;;;  stante-is-os-x) "stante-lib-platform" "stante-lib-platform.el"
;;;;;;  (20710 60048))
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

;;;### (autoloads (stante-whitespace-mode-no-overlong-lines) "stante-lib-text"
;;;;;;  "stante-lib-text.el" (20710 59999))
;;; Generated autoloads from stante-lib-text.el

(autoload 'stante-whitespace-mode-no-overlong-lines "stante-lib-text" "\
Disable highlighting of overlong lines in `whitespace-mode'.

Affects the current buffer only.

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
