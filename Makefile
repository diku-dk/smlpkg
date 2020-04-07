prefix ?= .
INSTALLDIR ?= $(prefix)/bin
INSTALL ?= install
OS=$(shell uname -s | tr '[:upper:]' '[:lower:]')

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
	rm -rf *~ .*~ bin smlpkg-bin-*

src/smlpkg:
	$(MAKE) -C src all

# -----------------------------------------------------
# Target for building binary distribution for smlpkg
# -----------------------------------------------------

BIN_DIST_DIR=smlpkg-bin-dist-$(OS)
.PHONY: bin_dist
bin_dist: src/smlpkg
	rm -rf smlpkg-bin-dist-*
	mkdir $(BIN_DIST_DIR)
	$(MAKE) install INSTALLDIR=$(BIN_DIST_DIR)/bin/
	$(INSTALL) LICENSE $(BIN_DIST_DIR)/
	echo 'Binary package for smlpkg.' > $(BIN_DIST_DIR)/README
	echo 'The sources are available at http://github.com/diku-dk/smlpkg' >> $(BIN_DIST_DIR)/README
	tar czvf $(BIN_DIST_DIR).tgz $(BIN_DIST_DIR)
