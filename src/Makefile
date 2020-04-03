MLCOMP ?= mlkit

SMLFILES=$(shell find . -name '*.sml') $(shell find . -name '*.sig') $(shell find . -name '*.mlb')

.PHONY: all
all: smlpkg

smlpkg: pkg.mlb $(SMLFILES)
	$(MLCOMP) -output $@ $<

.PHONY: test
test:
	$(MAKE) -C semver test
	$(MAKE) -C manifest test
	$(MAKE) -C solve test
	$(MAKE) -C util test

.PHONY: clean
clean:
	rm -rf *~ MLB run pkg.exe smlpkg
	$(MAKE) -C semver clean
	$(MAKE) -C manifest clean
	$(MAKE) -C solve clean
	$(MAKE) -C parsecomb clean
	$(MAKE) -C util clean
