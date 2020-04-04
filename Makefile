
.PHONY: all
all:
	$(MAKE) -C src all

.PHONY: test
test:
	$(MAKE) -C src test
	$(MAKE) -C pkgtests test

.PHONY: clean
clean:
	$(MAKE) -C src clean
	rm -f *~ .*~
