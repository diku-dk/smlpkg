prefix ?= .
INSTALLDIR ?= $(prefix)/bin
INSTALL ?= install

.PHONY: all
all: src/smlpkg

.PHONY:
install: src/smlpkg
	mkdir -p $(INSTALLDIR)
	$(INSTALL) $< $(INSTALLDIR)/

.PHONY: test
test:
	$(MAKE) -C src test
	$(MAKE) -C src/test test
	$(MAKE) -C pkgtests test

.PHONY: clean
clean:
	$(MAKE) -C src clean
	rm -rf *~ .*~ bin

src/smlpkg:
	$(MAKE) -C src all
