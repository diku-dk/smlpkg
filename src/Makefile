
.PHONY: test
test:
	$(MAKE) -C semver test
	$(MAKE) -C manifest test

.PHONY: clean
clean:
	rm -rf *~ MLB run
	$(MAKE) -C semver clean
	$(MAKE) -C manifest clean
	$(MAKE) -C parsecomb clean