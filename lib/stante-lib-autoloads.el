;;; stante-lib-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (stante-get-file-lines stante-set-file-contents
;;;;;;  stante-get-file-contents) "stante-lib-io" "stante-lib-io.el"
;;;;;;  (20561 59717))
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
;;;;;;  (20561 59740))
;;; Generated autoloads from stante-lib-lists.el

(autoload 'stante-merge-alists "stante-lib-lists" "\
Update the alist A with B.

Items in B replace items in A with matching `car'.

\(fn A B)" nil nil)

;;;***

;;;### (autoloads (stante-byte-recompile stante-update-autoload-file)
;;;;;;  "stante-lib-maintenance" "stante-lib-maintenance.el" (20561
;;;;;;  59890))
;;; Generated autoloads from stante-lib-maintenance.el

(defconst stante-autoload-file (concat stante-lib-dir "stante-lib-autoloads.el") "\
Location of the autoload file for the Stante Pede Library.")

(autoload 'stante-update-autoload-file "stante-lib-maintenance" "\
Update the autoload file of Stante Pede.

\(fn)" t nil)

(autoload 'stante-byte-recompile "stante-lib-maintenance" "\
Byte-compile all modules of Stante pede.

\(fn)" t nil)

;;;***

;;;### (autoloads (package-install-if-needed) "stante-lib-package"
;;;;;;  "stante-lib-package.el" (20561 59802))
;;; Generated autoloads from stante-lib-package.el

(autoload 'package-install-if-needed "stante-lib-package" "\
Install the package named NAME, unless it is already installed.

\(fn NAME)" nil nil)

;;;***

;;;### (autoloads (stante-is-windows stante-is-gnome stante-is-kde
;;;;;;  stante-is-os-x) "stante-lib-platform" "stante-lib-platform.el"
;;;;;;  (20561 59819))
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

;;;### (autoloads (stante-string-trim) "stante-lib-strings" "stante-lib-strings.el"
;;;;;;  (20561 59832))
;;; Generated autoloads from stante-lib-strings.el

(autoload 'stante-string-trim "stante-lib-strings" "\
Remove leading and trailing whitespace from STR.

\(fn STR)" nil nil)

;;;***

;;;### (autoloads nil nil ("stante-autoloads.el") (20561 59897 496678))

;;;***

(provide 'stante-lib-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; stante-lib-autoloads.el ends here
