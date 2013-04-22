.PHONY: all doc library install
OCAMLBUILD = ocamlbuild -use-ocamlfind 
OCAMLDOC = ocamlfind ocamldoc 
OCPPACK = ../tools/ocp-pack-1.0.1/ocp-pack

all:
	$(OCAMLBUILD) debug.otarget

native:
	$(OCAMLBUILD) native.otarget

doc:
	mkdir -p doc
	$(OCPPACK) -o doc/Modules.ml Modules/*.ml
	$(OCPPACK) -o doc/Misc.ml Misc/*.ml
	$(OCPPACK) -o doc/OpenFlow0x01.ml OpenFlow0x01/Sig.ml OpenFlow0x01/Parser.ml \
		OpenFlow0x01/Platform.ml
	$(OCAMLDOC) -d doc/ -html -I _build/extracted -I doc -I _build/mininet \
		-I _build -package lwt -package cstruct -syntax camlp4o -package camlp4 \
		-package cstruct.syntax -package lwt.syntax \
		*.mli doc/*.ml


test: _build/Test.d.byte
	$<

clean:
	rm -rf _build
	rm -rf extracted

library: 
	$(OCAMLBUILD) library.otarget

install: library
	ocamlfind install desmoines META *.cmi *.cma

uninstall:
	ocamlfind remove desmoines