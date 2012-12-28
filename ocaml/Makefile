.PHONY = all doc
OCAMLBUILD = ocamlbuild -use-ocamlfind

all: extracted doc
	$(OCAMLBUILD) main.d.byte

doc:
	$(OCAMLBUILD) doc.docdir/index.html


clean:
	rm -rf _build
	rm -rf extracted
