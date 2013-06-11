EMACS = emacs
CARTON = carton

export EMACS

SRCS = init.el
OBJECTS = $(SRCS:.el=.elc)

.PHONY: all
all: compile

.PHONY: update
update: Carton
	EMACS=$(EMACS) $(CARTON) update

.PHONY: clean-packages
clean-packages:
	rm -rf elpa

.PHONY: compile
compile : $(OBJECTS)

.PHONY: clean
clean :
	rm -f $(OBJECTS)

elpa : Carton
	$(CARTON) install

%.elc : %.el elpa
	$(EMACS) -f package-initialize -Q --batch $(EMACSFLAGS) -f batch-byte-compile $<
