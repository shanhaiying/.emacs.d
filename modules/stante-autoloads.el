;;; stante-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (stante-byte-recompile stante-update-autoload-file
;;;;;;  stante-get-file-lines stante-set-file-contents stante-get-file-contents
;;;;;;  stante-merge-alists stante-string-trim package-install-if-needed)
;;;;;;  "stante-helper" "stante-helper.el" (20553 55704))
;;; Generated autoloads from stante-helper.el

(autoload 'package-install-if-needed "stante-helper" "\
Install the package named NAME, unless it is already installed.

\(fn NAME)" nil nil)

(autoload 'stante-string-trim "stante-helper" "\
Remove leading and trailing whitespace from STR.

\(fn STR)" nil nil)

(autoload 'stante-merge-alists "stante-helper" "\
Update the alist A with B.

Items in B replace items in A with matching `car'.

\(fn A B)" nil nil)

(autoload 'stante-get-file-contents "stante-helper" "\
Return the contents of the file FILENAME.

\(fn FILENAME)" nil nil)

(autoload 'stante-set-file-contents "stante-helper" "\
Set the contents of the file FILENAME.

Create the file FILENAME if it does not exist, or completely
overwrite it if it does.

\(fn FILENAME CONTENTS)" nil nil)

(autoload 'stante-get-file-lines "stante-helper" "\
Return a list of lines of file FILENAME.

\(fn FILENAME)" nil nil)

(autoload 'stante-update-autoload-file "stante-helper" "\
Update the autoload file of Stante Pede.

\(fn)" t nil)

(autoload 'stante-byte-recompile "stante-helper" "\
Byte-compile all modules of Stante pede.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("stante-editor.el" "stante-emacs-lisp.el"
;;;;;;  "stante-german.el" "stante-git.el" "stante-osx.el" "stante-programming.el"
;;;;;;  "stante-sh.el" "stante-ui.el") (20553 56858 978166))

;;;***

(provide 'stante-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; stante-autoloads.el ends here
