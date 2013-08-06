EMACS = emacs
CASK = cask

export EMACS

SRCS = init.el
OBJECTS = $(SRCS:.el=.elc)
PKGDIR := $(shell cask package-directory)

.PHONY: all
all: compile

.PHONY: update
update: Cask
	$(CASK) update

.PHONY: clean-packages
clean-packages:
	rm -rf $(PKGDIR)

.PHONY: compile
compile : $(OBJECTS)

.PHONY: clean
clean :
	rm -f $(OBJECTS)

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACS) -f package-initialize -Q --batch $(EMACSFLAGS) -f batch-byte-compile $<
