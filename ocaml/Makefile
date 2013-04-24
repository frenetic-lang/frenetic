.PHONY: all debug native doc library install uninstall clean reinstall

OCAMLBUILD = ocamlbuild -use-ocamlfind 

# Note that these are only the .mli files at the top-level.
MLIFILES = $(shell ls _build/*.mli)
CMIFILES = $(MLIFILES:.mli=.cmi)

all: debug doc library

debug:
	$(OCAMLBUILD) debug.otarget

native:
	$(OCAMLBUILD) native.otarget

doc:
	$(OCAMLBUILD) doc.docdir/index.html

test: _build/Test.d.byte
	$<

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
	ocamlfind install desmoines META *.cmi *.cma 

uninstall:
	ocamlfind remove desmoines

reinstall: library
	ocamlfind remove desmoines
	ocamlfind install desmoines META _build/*/*.cmi _build/*/*.cma _build/*.cmi _build/*.cma
