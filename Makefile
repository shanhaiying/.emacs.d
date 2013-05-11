EMACS = emacs
CARTON = carton

SRCS = init.el
OBJECTS = $(SRCS:.el=.elc)

.PHONY: all
all: deps compile

.PHONY: deps
deps :
	EMACS=$(EMACS) $(CARTON) install
	EMACS=$(EMACS) $(CARTON) update

.PHONY: compile
compile : $(OBJECTS)

.PHONY: clean
clean :
	rm -f $(OBJECTS) lib/stante-autoloads.el

%.elc : %.el
	$(EMACS) -f package-initialize \
		--no-site-file --no-site-lisp --batch $(EMACSFLAGS) \
		-f batch-byte-compile $<
