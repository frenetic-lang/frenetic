.PHONY: all debug native doc library install uninstall clean reinstall

OCAMLBUILD = ocamlbuild -use-ocamlfind 

# Note that these are only the .mli files at the top-level.
MLIFILES = $(shell ls _build/*.mli)
CMIFILES = $(MLIFILES:.mli=.cmi)

all: debug doc newuninstall newinstall

debug:
	$(OCAMLBUILD) debug.otarget

native:
	$(OCAMLBUILD) native.otarget

verify:
	$(OCAMLBUILD) verify.otarget

doc:
	$(OCAMLBUILD) doc.docdir/index.html

clean:
	rm -rf _build
	rm -rf extracted

library: 
	$(OCAMLBUILD) library.otarget

newinstall:
	ocamlfind install NetCoreLib \
		META $(MLIFILES) $(CMIFILES) NetCoreLib.cma

newuninstall:
	ocamlfind remove NetCoreLib

install: library 
	ocamlfind install desmoines desmoines_lib/META *.cmi *.cma 

uninstall:
	ocamlfind remove desmoines

reinstall: library
	ocamlfind remove desmoines
	ocamlfind install desmoines desmoines_lib/META _build/*/*.cmi _build/*/*.cma _build/*.cmi _build/*.cma
