.PHONY: all clean extract

all: Makefile.coq
	mkdir -p ../ocaml/extracted
	$(MAKE) -f Makefile.coq

doc:
	mkdir -p coqdoc
	find . -name "*.v" | xargs coqdoc -R . Coqnets -d coqdoc -v

Makefile.coq: Makefile
	coq_makefile Common Bag Word Network OpenFlow Wildcard Pattern Classifier NetCore FwOF OpenFlow13 Extraction -o Makefile.coq

clean: Makefile.coq
	$(MAKE) -f Makefile.coq clean
	rm -f Makefile.coq

extract: Makefile.coq
	$(MAKE) -f Makefile.coq Examples
