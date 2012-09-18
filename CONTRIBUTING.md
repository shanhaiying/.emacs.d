Contributing
============

If discovered bugs and issues, have ideas for improvements or new features, or
want to contribute a new module, please report to the [issue tracker][1] or fork
the repository and send a pull request, but respect the following guidelines.

Issue reporting
---------------

* Check that the issue has not already been reported.
* Check that the issue has not already been fixed in the latest code.
* Open an issue with a clear title and description in grammatically correct,
  complete sentences.

Pull requests
-------------

* Read [how to properly contribute to open source projects on GitHub][2].
* Use a topic branch to easily amend a pull request later, if necessary.
* Write [good commit messages][3].
* Use the same coding style and spacing.  At best, use the Stante Pede settings
  to write Emacs Lisp code.
* Use [lexical binding][4].
* Obey the conventions for [library headers][5].  Add your name and email
  address to the ``Author`` field.  Add ``URL`` field pointing to
  https://gihub.com/lunaryorn/stantepede.git, and ``Keywords`` as appropriate.
* Add a `;;; Commentary section` that explains all features and key bindings.
* Verify your Emacs Lisp code with `checkdoc` (`C-c d`).
* Open a [pull request][6] that relates to but one subject with a clear title
  and description in grammatically correct, complete sentences.


[1]: https://github.com/lunaryorn/stante-pede/issues
[2]: http://gun.io/blog/how-to-github-fork-branch-and-pull-request
[3]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[4]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Lexical-Binding.html
[5]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html
[6]: https://help.github.com/articles/using-pull-requests
