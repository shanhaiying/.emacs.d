;;; stante-lib-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (stante-TeX-select-view-programs) "stante-lib-TeX-viewers"
;;;;;;  "stante-lib-TeX-viewers.el" (20605 27471))
;;; Generated autoloads from stante-lib-TeX-viewers.el

(autoload 'stante-TeX-select-view-programs "stante-lib-TeX-viewers" "\
Select the best view programs for the current platform.

\(fn)" nil nil)

;;;***

;;;### (autoloads (stante-get-file-lines stante-set-file-contents
;;;;;;  stante-get-file-contents) "stante-lib-io" "stante-lib-io.el"
;;;;;;  (20605 27399))
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

;;;### (autoloads (stante-merge-alists) "stante-lib-lists" "stante-lib-lists.el"
;;;;;;  (20605 27399))
;;; Generated autoloads from stante-lib-lists.el

(autoload 'stante-merge-alists "stante-lib-lists" "\
Update the alist A with B.

Items in B replace items in A with matching `car'.

\(fn A B)" nil nil)

;;;***

;;;### (autoloads (after) "stante-lib-load" "stante-lib-load.el"
;;;;;;  (20605 27399))
;;; Generated autoloads from stante-lib-load.el

(autoload 'after "stante-lib-load" "\
Evaluate FORMS after FILE is loaded.

FILE may be a named feature, see `eval-after-load'.

\(fn FILE &rest FORMS)" nil (quote macro))

(put 'after 'lisp-indent-function '1)

;;;***

;;;### (autoloads (stante-report-issue stante-byte-recompile stante-update-autoload-file)
;;;;;;  "stante-lib-maintenance" "stante-lib-maintenance.el" (20605
;;;;;;  27399))
;;; Generated autoloads from stante-lib-maintenance.el

(defconst stante-autoload-file (concat stante-lib-dir "stante-lib-autoloads.el") "\
Location of the autoload file for the Stante Pede Library.")

(autoload 'stante-update-autoload-file "stante-lib-maintenance" "\
Update the autoload file of Stante Pede.

\(fn)" t nil)

(autoload 'stante-byte-recompile "stante-lib-maintenance" "\
Byte-compile all modules of Stante pede.

\(fn)" t nil)

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
;;;;;;  "stante-lib-os-x" "stante-lib-os-x.el" (20605 27485))
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

;;;### (autoloads (package-require package-need) "stante-lib-package"
;;;;;;  "stante-lib-package.el" (20605 27399))
;;; Generated autoloads from stante-lib-package.el

(autoload 'package-need "stante-lib-package" "\
Ensure that the package NAME is available.

If the package is not available, it is automatically installed.

\(fn NAME)" nil nil)

(autoload 'package-require "stante-lib-package" "\
Ensure that package NAME is available and require the feature NAME.

\(fn NAME)" nil nil)

;;;***

;;;### (autoloads (stante-is-windows stante-is-gnome stante-is-kde
;;;;;;  stante-is-os-x) "stante-lib-platform" "stante-lib-platform.el"
;;;;;;  (20605 27399))
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

;;;### (autoloads (stante-ido-goto-symbol) "stante-lib-programming"
;;;;;;  "stante-lib-programming.el" (20605 27399))
;;; Generated autoloads from stante-lib-programming.el

(autoload 'stante-ido-goto-symbol "stante-lib-programming" "\
Refresh imenu and jump to a place in the buffer using Ido.

\(fn &optional SYMBOL-LIST)" t nil)

;;;***


;;;### (autoloads nil nil ("stante-lib-TeX-biber.el" "stante-lib-TeX-latexmk.el")
;;;;;;  (20588 11435 623680))

;;;***

;;;### (autoloads nil nil ("stante-lib-TeX-biber.el" "stante-lib-TeX-latexmk.el")
;;;;;;  (20605 27491 243404))

;;;***

(provide 'stante-lib-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; stante-lib-autoloads.el ends here
