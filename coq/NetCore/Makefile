MODULES := \
  NetCoreEval \
  NetCoreCompiler \
	Verifiable \
  NetCoreTheorems \
  NetCoreController

VS := $(MODULES:%=%.v)

.PHONY: all clean

all : Makefile.coq
	$(MAKE) -f Makefile.coq

Makefile.coq: Makefile $(VS)
	coq_makefile -R ".." Coqnets $(VS) -o Makefile.coq

clean: Makefile.coq
	$(MAKE) -f Makefile.coq clean
	rm -f Makefile.coq
