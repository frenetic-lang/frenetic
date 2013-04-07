MODULES := \
  FwOFExtractableSignatures \
  FwOFExtractableController \
  FwOFSignatures \
  FwOFSimpleController \
  FwOFMachine \
	FwOFSafeWire \
  FwOFSimpleControllerLemmas \
  FwOFWellFormednessLemmas \
  FwOFWellFormedness \
  FwOFWeakSimulation1 \
	FwOFWeakSimulation2 \
  FwOFRelationDefinitions \
  FwOFBisimulation \
  FwOFNetworkAtoms \

VS := $(MODULES:%=%.v)

.PHONY: all clean

all: Makefile.coq
	$(MAKE) -f Makefile.coq

Makefile.coq: Makefile $(VS)
	coq_makefile -R ".." Coqnets $(VS) -o Makefile.coq

clean: Makefile.coq
	$(MAKE) -f Makefile.coq clean
	rm -f Makefile.coq
