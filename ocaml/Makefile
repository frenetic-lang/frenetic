.PHONY: all doc
OCAMLBUILD = ocamlbuild -use-ocamlfind 

all: extracted doc
	$(OCAMLBUILD) Main.d.byte
	$(OCAMLBUILD) Test.d.byte
	$(OCAMLBUILD) Verify.d.byte

doc:
	$(OCAMLBUILD) doc.docdir/index.html

test: _build/Test.d.byte
	$<

clean:
	rm -rf _build
	rm -rf extracted
