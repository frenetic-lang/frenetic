MODULES := TotalOrder OrderedLists Bag2Defs Bag2Notations Bag2Lemmas \
  Bag2Tactics Bag2
VS := $(MODULES:%=%.v)

.PHONY: all clean

all: Makefile.coq
	$(MAKE) -f Makefile.coq

Makefile.coq: Makefile $(VS)
	coq_makefile -R ".." Coqnets $(VS) -o Makefile.coq

clean: Makefile.coq
	$(MAKE) -f Makefile.coq clean
	rm -f Makefile.coq
