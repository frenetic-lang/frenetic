.PHONY: all clean

OCAMLBUILD = ocamlbuild -use-ocamlfind

all:
	$(OCAMLBUILD) -cflag -ppopt -cflag -lwt-debug Main.d.byte

clean:
	$(OCAMLBUILD) -clean
