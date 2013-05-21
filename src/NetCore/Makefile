.PHONY: all debug native clean

OCAMLBUILD = ocamlbuild -use-ocamlfind

all: debug native  

debug:
	$(OCAMLBUILD) -cflag -ppopt -cflag -lwt-debug debug.otarget

native:
	$(OCAMLBUILD) native.otarget

clean:
	rm -rf _build *.d.byte *.native 
