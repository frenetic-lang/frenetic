.PHONY = all doc
OCAMLBUILD = ocamlbuild -use-ocamlfind

all: extracted doc
	$(OCAMLBUILD) main.d.byte
	$(OCAMLBUILD) test.d.byte

doc:
	$(OCAMLBUILD) doc.docdir/index.html

