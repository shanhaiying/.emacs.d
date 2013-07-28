EMACS = emacs
CARTON = carton

export EMACS

SRCS = init.el
OBJECTS = $(SRCS:.el=.elc)
PKGDIR := $(shell carton package-directory)

.PHONY: all
all: compile

.PHONY: update
update: Carton
	EMACS=$(EMACS) $(CARTON) update

.PHONY: clean-packages
clean-packages:
	rm -rf $(PKGDIR)

.PHONY: compile
compile : $(OBJECTS)

.PHONY: clean
clean :
	rm -f $(OBJECTS)

$(PKGDIR) : Carton
	$(CARTON) install
	touch $(PKGDIR)

%.elc : %.el $(PKGDIR)
	$(CARTON) exec $(EMACS) -f package-initialize -Q --batch $(EMACSFLAGS) -f batch-byte-compile $<
