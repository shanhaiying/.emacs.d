;;; stante-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (stante-get-file-lines stante-set-file-contents
;;;;;;  stante-get-file-contents) "stante-io" "stante-io.el" (20557
;;;;;;  64604))
;;; Generated autoloads from stante-io.el

(autoload 'stante-get-file-contents "stante-io" "\
Return the contents of the file FILENAME.

\(fn FILENAME)" nil nil)

(autoload 'stante-set-file-contents "stante-io" "\
Set the contents of the file FILENAME.

Create the file FILENAME if it does not exist, or completely
overwrite it if it does.

\(fn FILENAME CONTENTS)" nil nil)

(autoload 'stante-get-file-lines "stante-io" "\
Return a list of lines of file FILENAME.

\(fn FILENAME)" nil nil)

;;;***

;;;### (autoloads (stante-merge-alists) "stante-lists" "stante-lists.el"
;;;;;;  (20557 64604))
;;; Generated autoloads from stante-lists.el

(autoload 'stante-merge-alists "stante-lists" "\
Update the alist A with B.

Items in B replace items in A with matching `car'.

\(fn A B)" nil nil)

;;;***

;;;### (autoloads (stante-byte-recompile stante-update-autoload-file)
;;;;;;  "stante-maintenance" "stante-maintenance.el" (20557 64604))
;;; Generated autoloads from stante-maintenance.el

(defconst stante-autoload-file (concat stante-lib-dir "stante-autoloads.el") "\
Location of the autoload file for the Stante Pede Library.")

(autoload 'stante-update-autoload-file "stante-maintenance" "\
Update the autoload file of Stante Pede.

\(fn)" t nil)

(autoload 'stante-byte-recompile "stante-maintenance" "\
Byte-compile all modules of Stante pede.

\(fn)" t nil)

;;;***

;;;### (autoloads (package-install-if-needed) "stante-package" "stante-package.el"
;;;;;;  (20557 64604))
;;; Generated autoloads from stante-package.el

(autoload 'package-install-if-needed "stante-package" "\
Install the package named NAME, unless it is already installed.

\(fn NAME)" nil nil)

;;;***

;;;### (autoloads (stante-is-windows stante-is-gnome stante-is-kde
;;;;;;  stante-is-os-x) "stante-platform" "stante-platform.el" (20559
;;;;;;  18967))
;;; Generated autoloads from stante-platform.el

(autoload 'stante-is-os-x "stante-platform" "\
Return t if running on OS X, or nil otherwise.

\(fn)" nil nil)

(autoload 'stante-is-kde "stante-platform" "\
Return t if running in a KDE session, or nil otherwise.

\(fn)" nil nil)

(autoload 'stante-is-gnome "stante-platform" "\
Return t if running on Gnome, or nil otherwise.

\(fn)" nil nil)

(autoload 'stante-is-windows "stante-platform" "\
Return t if running natively on Windows, or nil otherwise.

Return nil if running in cygwin or under MS-DOS.

\(fn)" nil nil)

;;;***

;;;### (autoloads (stante-string-trim) "stante-strings" "stante-strings.el"
;;;;;;  (20557 64604))
;;; Generated autoloads from stante-strings.el

(autoload 'stante-string-trim "stante-strings" "\
Remove leading and trailing whitespace from STR.

\(fn STR)" nil nil)

;;;***

(provide 'stante-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; stante-autoloads.el ends here
