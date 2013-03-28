.PHONY: all doc
OCAMLBUILD = ocamlbuild -use-ocamlfind 

all: extracted doc
	$(OCAMLBUILD) Main_OpenFlow0x04.d.byte
	$(OCAMLBUILD) Main_OpenFlow0x01.d.byte
	$(OCAMLBUILD) Test.d.byte
	$(OCAMLBUILD) Verify.d.byte
	$(OCAMLBUILD) Main_Verified.d.byte

native:
	$(OCAMLBUILD) Main_OpenFlow0x01.native
	$(OCAMLBUILD) Main_Verified.native

doc:
	$(OCAMLBUILD) doc.docdir/index.html

test: _build/Test.d.byte
	$<

clean:
	rm -rf _build
	rm -rf extracted
