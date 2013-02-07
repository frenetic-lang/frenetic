.PHONY = all doc
OCAMLBUILD = ocamlbuild -use-ocamlfind -tag debug -ocamlc '-syntax camlp4o -package lwt.syntax -ppopt -lwt-debug'

all: extracted doc
	$(OCAMLBUILD) Main.d.byte
	$(OCAMLBUILD) Test.d.byte

doc:
	$(OCAMLBUILD) doc.docdir/index.html


clean:
	rm -rf _build
	rm -rf extracted
