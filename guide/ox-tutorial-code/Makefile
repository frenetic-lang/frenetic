.PHONY: dummy all clean .force

OCAMLBUILD = ocamlbuild -use-ocamlfind

dummy:
	@echo "To compile foo.ml, type:"
	@echo "$ make foo.d.byte"
	@echo
	@echo "If you really want to build everything, use make all."

all: $(patsubst %.ml,%.d.byte,$(wildcard *.ml))

%.d.byte: .force
	$(OCAMLBUILD) -cflag -ppopt -cflag -lwt-debug $@

clean:
	$(OCAMLBUILD) -clean
