EMACS = emacs
CARTON = carton

SRCS = init.el
OBJECTS = $(SRCS:.el=.elc)

.PHONY: all
all: packages update-packages compile

.PHONY: packages
packages : Carton
	EMACS=$(EMACS) $(CARTON) install

.PHONY: update-packages
update-packages: Carton
	EMACS=$(EMACS) $(CARTON) update

.PHONY: clean-packages
clean-packages:
	rm -rf elpa

.PHONY: compile
compile : $(OBJECTS)

.PHONY: clean
clean :
	rm -f $(OBJECTS)

%.elc : %.el
	$(EMACS) -f package-initialize -Q --batch $(EMACSFLAGS) -f batch-byte-compile $<
