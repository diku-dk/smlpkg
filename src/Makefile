PKGVERSION=$(shell cat ../version.txt)

MLCOMP ?= mlkit

SMLFILES=$(shell find . -name '*.sml') $(shell find . -name '*.sig') $(shell find . -name '*.mlb')

GITVERSION=$(shell git describe --abbrev --dirty --always --tags)

.PHONY: all
all: smlpkg futpkg

smlpkg: smlpkg.mlb $(SMLFILES) version.gen.sml
	$(MLCOMP) -output $@ $<

futpkg: futpkg.mlb $(SMLFILES)
	$(MLCOMP) -output $@ $<

.PHONY: test
test:
	$(MAKE) -C semver test
	$(MAKE) -C manifest test
	$(MAKE) -C solve test
	$(MAKE) -C util test

.PHONY: remotetest
remotetest:
	$(MAKE) -C test test

version.gen.sml: Makefile ../version.txt
	echo 'structure Version = struct' > $@
	echo "  val version = \"$(PKGVERSION)\"" >> $@
	echo "  val gitversion = \"$(GITVERSION)\"" >> $@
	echo "end" >> $@

.PHONY: clean
clean:
	rm -rf *~ MLB run pkg.exe smlpkg version.gen.sml
	$(MAKE) -C semver clean
	$(MAKE) -C manifest clean
	$(MAKE) -C solve clean
	$(MAKE) -C parsecomb clean
	$(MAKE) -C util clean
	$(MAKE) -C test clean
