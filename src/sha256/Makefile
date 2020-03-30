
MLCOMP ?= mlkit

.PHONY: test
test: test.exe
	./test.exe

test.exe: test_sha256.sml test_sha256.mlb
	$(MLCOMP) -output test.exe test_sha256.mlb

.PHONY: all
all:
	$(MLCOMP) sha256.mlb

.PHONY: clean
clean:
	find . -name '*~' | xargs rm -f
	find . -name 'MLB' | xargs rm -rf
	rm -f test.exe
