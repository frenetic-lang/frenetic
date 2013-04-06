MODULES := \
  FwOFSignatures \
  FwOFWellFormednessLemmas \
  FwOFWellFormedness \
  FwOFWeakSimulation1 \
	FwOFWeakSimulation2 \
  FwOFMachine \
  FwOFSimpleController \
  FwOFRelationDefinitions \
  FwOFBisimulation \
  FwOFNetworkAtoms \
  FwOFSimpleExtractableController \
	FwOFSafeWire \
  FwOFSimpleControllerLemmas \

VS := $(MODULES:%=%.v)

.PHONY: all clean

all: Makefile.coq
	$(MAKE) -f Makefile.coq

Makefile.coq: Makefile $(VS)
	coq_makefile -R ".." Coqnets $(VS) -o Makefile.coq

clean: Makefile.coq
	$(MAKE) -f Makefile.coq clean
	rm -f Makefile.coq
