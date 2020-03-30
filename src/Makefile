
.PHONY: test
test:
	$(MAKE) -C semver test
	$(MAKE) -C manifest test
	$(MAKE) -C sha256 test

.PHONY: clean
clean:
	rm -rf *~ MLB run
	$(MAKE) -C semver clean
	$(MAKE) -C manifest clean
	$(MAKE) -C parsecomb clean
	$(MAKE) -C sha256 clean
