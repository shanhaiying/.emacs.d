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
	$(MAKE) clean compile

.PHONY: clean-packages
clean-packages:
	rm -rf $(PKGDIR)

.PHONY: profile
profile:
	$(EMACS) -Q -l ./vendor/profile-dotemacs.el \
		--eval '(setq profile-dotemacs-file "./init.el")' \
		-f profile-dotemacs

.PHONY: compile
compile : $(OBJECTS)

.PHONY: clean
clean :
	rm -f $(OBJECTS)

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACS) -Q --batch \
		--eval '(setq package-user-dir "$(PKGDIR)")' -f package-initialize \
		$(EMACSFLAGS) \
		-f batch-byte-compile $<
