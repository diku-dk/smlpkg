MLCOMP ?= mlkit

.PHONY: test
test:
	$(MLCOMP) -output test.exe test.mlb
	$(MAKE) smallclean
	./test.exe

.PHONY: smallclean
smallclean:
	rm -rf test_zip/lib test_zip/lib_tmp~

.PHONY: clean
clean: smallclean
	rm -rf *~ MLB run *.exe
