.PHONY: all verify doc Verify.d.byte 
OCAMLBUILD = ocamlbuild -use-ocamlfind -tag debug -ocamlc '-syntax camlp4o -package lwt.syntax -ppopt -lwt-debug'

all: extracted doc Test.d.byte Main.d.byte

verify: Verify.d.byte

Test.d.byte:
	$(OCAMLBUILD) Test.d.byte

Main.d.byte:
	$(OCAMLBUILD) Main.d.byte

Verify.d.byte:
	$(OCAMLBUILD) Verify.d.byte

doc:
	$(OCAMLBUILD) doc.docdir/index.html

test: Test.d.byte
	./Test.d.byte


clean:
	rm -rf _build
	rm -rf extracted
