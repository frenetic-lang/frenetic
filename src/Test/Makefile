.PHONY: all clean

OCAMLBUILD = ocamlbuild -use-ocamlfind -log log

OCAMLFIND = ocamlfind

all: 
	$(OCAMLBUILD) -cflag -ppopt -cflag -lwt-debug Test.d.byte

clean:
	$(OCAMLBUILD) -clean
