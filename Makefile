
OCAMLBUILD = ocamlbuild -use-ocamlfind 

all:
	$(OCAMLBUILD) main.native

clean:
	$(OCAMLBUILD) -clean

.PHONY: all clean
