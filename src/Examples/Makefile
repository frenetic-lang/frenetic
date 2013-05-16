.PHONY: all debug native clean ctags

OCAMLBUILD = ocamlbuild -use-ocamlfind

all: debug

debug:
	$(OCAMLBUILD) -cflag -ppopt -cflag -lwt-debug debug.otarget

native:
	$(OCAMLBUILD) native.otarget

clean:
	rm -rf _build

ctags: $(shell ls *.mli) $(shell ls *.ml)
	ctags test 2>/dev/null >/dev/null && rm -f tags && ctags *.mli *.ml

