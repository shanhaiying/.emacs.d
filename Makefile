EMACS = emacs
CARTON = carton

LIB_SRCS = lib/stante-TeX-viewers.el \
	lib/stante-editor.el \
	lib/stante-emacs-lisp.el \
	lib/stante-files.el \
	lib/stante-markdown.el \
	lib/stante-os-x.el \
	lib/stante-programming.el \
	lib/stante-text.el

SRCS = init.el $(LIB_SRCS)
OBJECTS = $(SRCS:.el=.elc)

.PHONY: all
all: deps compile autoloads

.PHONY: deps
deps :
	EMACS=$(EMACS) $(CARTON) install
	EMACS=$(EMACS) $(CARTON) update

.PHONY: compile
compile : $(OBJECTS)

.PHONY: autoloads
autoloads : lib/stante-autoloads.el

.PHONY: clean
clean :
	rm -f $(OBJECTS) lib/stante-autoloads.el

lib/stante-autoloads.el : $(LIB_SRCS)
	$(EMACS) --no-site-file --no-site-lisp --batch $(EMACSFLAGS) \
		--eval "(let ((generated-autoload-file \"$(PWD)/lib/stante-autoloads.el\")) (update-directory-autoloads \"lib\"))"

%.elc : %.el
	$(EMACS) -L lib/ -f package-initialize \
		--no-site-file --no-site-lisp --batch $(EMACSFLAGS) \
		-f batch-byte-compile $<
