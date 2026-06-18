MLCOMP ?= mlkit
MLCOMP_FLAGS ?=

ifeq ($(MLCOMP), polymlb)
MLCOMP_FLAGS=-main reportAndExit
endif

.PHONY: test
test:
	$(MLCOMP) $(MLCOMP_FLAGS) -output test.exe test.mlb
	./test.exe

.PHONY: clean
clean:
	rm -rf *~ MLB run *.exe
