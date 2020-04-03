MLCOMP ?= mlkit

.PHONY: all
all: pkg.exe

pkg.exe: pkg.mlb pkginfo.sig pkginfo.sml
	$(MLCOMP) -output $@ $<

.PHONY: test
test:
	$(MAKE) -C semver test
	$(MAKE) -C manifest test
	$(MAKE) -C sha256 test
	$(MAKE) -C util test

.PHONY: clean
clean:
	rm -rf *~ MLB run pkg.exe
	$(MAKE) -C semver clean
	$(MAKE) -C manifest clean
	$(MAKE) -C parsecomb clean
	$(MAKE) -C sha256 clean
	$(MAKE) -C util clean
