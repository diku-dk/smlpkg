MLCOMP ?= mlkit

.PHONY: test
test:
	$(MLCOMP) -output test.exe test.mlb
	./test.exe

.PHONY: clean
clean:
	rm -rf *~ MLB run *.exe
