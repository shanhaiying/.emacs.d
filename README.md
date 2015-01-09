.emacs.d
========

My Emacs configuration

Setup
-----

```console
$ git clone https://github.com/lunaryorn/.emacs.d.git ~/.emacs.d
```

### OS X support ###

```console
$ brew install trash coreutils
```

### Spell checking ###

```console
$ brew install aspell --with-lang-de --with-lang-en
```

### Python support ###

```console
$ pip install -U --user virtualenv pylint ipython
```

### Haskell support ###

```console
$ cabal install hlint hasktags hoogle present
```

### Shell scripting ###

```console
$ brew install shellcheck
```

### Markdown support ###

```console
$ brew install pandoc
```

### Ansible support ###

```console
$ brew install ansible
```

[Cask]: http://cask.readthedocs.org/en/latest/
[OpenOffice]: https://wiki.openoffice.org/wiki/Dictionaries

Usage
-----

My `init.el` only contains package configuration (with `use-package`) and Emacs
settings, but no functions.  I keep my functions in separate files in the
`lisp/` directory, which I load and configure with `use-package`.

All 3rd party packages come from MELPA or GNU ELPA.  I use the `:ensure` feature
of `use-package` to automatically install missing packages.

Notable packages
----------------

- **Package management**: [use-package](https://github.com/jwiegley/use-package)
  and [Paradox](https://github.com/Bruce-Connor/paradox)
- **Version control**: [Magit](https://github.com/magit/magit) and
  [Diff Hl](https://github.com/dgutov/diff-hl)
- **Fonts:** [Source Code Pro](https://github.com/adobe/source-code-pro), with
  [Unicode Fonts](https://github.com/rolandwalker/unicode-fonts)
- **Color theme**: [Solarized Light](https://github.com/bbatsov/solarized-emacs)
- **Mode line**: [Anzu](https://github.com/syohex/emacs-anzu), and
  [Fancy Battery](https://github.com/lunaryorn/fancy-battery.el)
- **Buffers**: [ibuffer-vc](https://github.com/purcell/ibuffer-vc)
- **Minibuffer**: [SMex](https://github.com/nonsequitur/smex),
  [Ido Ubiquituous](https://github.com/DarwinAwardWinner/ido-ubiquitous),
  [Ido Load Library](https://github.com/rolandwalker/ido-load-library),
  [Flx](https://github.com/lewang/flx) and
  [Ido Vertical Mode](https://github.com/gempesaw/ido-vertical-mode.el)
- **Visual guides**:
  [Page Break Lines](https://github.com/purcell/page-break-lines) and
  [Rainbow Delimiters](https://github.com/jlr/rainbow-delimiters)
- **Undo and killing**: [Undo Tree](http://www.dr-qubit.org/emacs.php#undo-tree),
  [Browse Kill Ring](https://github.com/browse-kill-ring/browse-kill-ring) and
  [Easy Kill](https://github.com/leoliu/easy-kill)
- **Region and editing**:
  [Expand Region](https://github.com/magnars/expand-region.el),
  [Multiple Cursors](https://github.com/magnars/multiple-cursors.el), and
  [Visual Regexp](https://github.com/benma/visual-regexp.el)
- **In-buffer navigation**:
  [ACE Jump Mode](https://github.com/winterTTr/ace-jump-mode) and
  [Imenu Anywhere](https://github.com/vitoshka/imenu-anywhere)
- **Syntax checking**: [Flycheck](http://flycheck.readthedocs.org) and
  [Flycheck Pos Tip](https://github.com/flycheck/flycheck-pos-tip)
- **Auto-completion**: [Company Mode](http://company-mode.github.io),
  [Company Math](https://github.com/vspinu/company-math)
- **Symbols**: [Highlight Symbol](https://github.com/nschum/highlight-symbol.el)
- **Project navigation**: [Projectile](https://github.com/bbatsov/projectile)
- **Search**: [Ag](https://github.com/Wilfred/ag.el) and
  [wgrep](https://github.com/mhayashi1120/Emacs-wgrep)
- **Lisp**: [Paredit](http://mumble.net/~campbell/emacs/paredit.html)
- **Emacs Lisp**: [Macrostep](https://github.com/joddie/macrostep),
  [Elisp Slime Nav](https://github.com/purcell/elisp-slime-nav) and
  [Flycheck Cask](https://github.com/flycheck/flycheck-cask)
- **Python**: [Anaconda Mode](https://github.com/proofit404/anaconda-mode) and
  [Company Anaconda](https://github.com/proofit404/company-anaconda)
- **Haskell**: [Haskell Mode](https://github.com/haskell/haskell-mode/),
  [Hi2](https://github.com/errge/hi2) and
  [Flycheck Haskell](https://github.com/flycheck/flycheck-haskell)
- **Rust**: [Flycheck Rust](https://github.com/flycheck/flycheck-rust)
- **OCaml**: [Tuareg](https://github.com/ocaml/tuareg/),
  [Merlin](https://github.com/the-lambda-church/merlin) and
  [Flycheck OCaml](https://github.com/flycheck/flycheck-ocaml)
- **Theorem proovers**: [Proof General](http://proofgeneral.inf.ed.ac.uk/)
- **Web**: [SX](https://github.com/vermiculus/sx.el)
