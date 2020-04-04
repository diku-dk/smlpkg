
.PHONY: test
test: test1.res test2.res test3.res
	cat $^

# -------------------------------
# The test cases
# -------------------------------

# test1 - result to be compared with futhark.pkg.test1.ok
futhark.pkg.test1.out: ../futpkg
	$(MAKE) smallclean
	../futpkg -v init github.com/melsman/test
	../futpkg -v add github.com/melsman/segmented 0.1.0
	mv futhark.pkg $@

# test2
futhark.pkg.test2.out: ../futpkg
	$(MAKE) smallclean
	../futpkg -v init github.com/melsman/test
	../futpkg -v add github.com/melsman/segmented 0.1.0
	../futpkg -v add github.com/athas/fut-baz 0.2.0
	../futpkg -v fmt
	mv futhark.pkg $@

# test3
futhark.pkg.test3.out: ../futpkg
	$(MAKE) smallclean
	../futpkg -v init github.com/melsman/test
	../futpkg -v add github.com/melsman/segmented 0.1.0
	../futpkg -v add github.com/athas/fut-baz 0.2.0
	../futpkg -v remove github.com/melsman/segmented
	mv futhark.pkg $@

# -------------------------------
# The generic test tooling
# -------------------------------

%.res: futhark.pkg.%.out
	@(diff -aq $< futhark.pkg.$*.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "OK:  $*" > $@ \
         ; else \
             if [ -e futhark.pkg.$*.ok ]; then \
                echo "ERR: $* - file $< differs from futhark.pkg.$*.ok"; \
                echo "ERR: $* - file $< differs from futhark.pkg.$*.ok" > $@ \
             ; else \
                echo "ERR: $* - file futhark.pkg.$*.ok does not exist"; \
                echo "ERR: $* - file futhark.pkg.$*.ok does not exist" > $@ \
             ; fi \
             ; exit 1 \
         ;fi)

../futpkg:
	$(MAKE) -C .. all

.PHONY: smallclean
smallclean:
	rm -rf *~ lib futhark.pkg sml.pkg

.PHONY: clean
clean:
	rm -rf *~ lib futhark.pkg sml.pkg *.res *.out
