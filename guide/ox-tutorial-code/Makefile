.PHONY: all clean .force

OCAMLBUILD = ocamlbuild -use-ocamlfind

all:

%.d.byte: .force
	$(OCAMLBUILD) -cflag -ppopt -cflag -lwt-debug $@

clean:
	$(OCAMLBUILD) -clean
