.PHONY = all
OCAMLBUILD = ocamlbuild -use-ocamlfind

all: extracted
	$(OCAMLBUILD) main.d.byte
	$(OCAMLBUILD) concTest.d.byte
