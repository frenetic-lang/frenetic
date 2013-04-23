.PHONY: all debug native doc library install uninstall clean
OCAMLBUILD = ocamlbuild -use-ocamlfind 

all: debug doc

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

install: library
	ocamlfind install desmoines META *.cmi *.cma

uninstall:
	ocamlfind remove desmoines
