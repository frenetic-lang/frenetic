.PHONY: all debug native uninstall install doc clean

OCAMLBUILD = ocamlbuild -use-ocamlfind

CTAGSFILES = $(shell ls *.ml) $(shell ls *.mli)

MLIFILES = _build/Syntax.mli 

CMIFILES = $(MLIFILES:.mli=.cmi)

all: debug native uninstall install doc

debug:
	$(OCAMLBUILD) -cflag -ppopt -cflag -lwt-debug debug.otarget

native:
	$(OCAMLBUILD) native.otarget

verify:
	$(OCAMLBUILD) verify.otarget

uninstall:
	ocamlfind remove NetCoreLib

install:
	ocamlfind install NetCoreLib META $(MLIFILES) $(CMIFILES) NetCoreLib.d.cma NetCoreLib.cmxa

doc:
	$(OCAMLBUILD) doc.docdir/index.html

clean:
	rm -rf _build *.d.byte *.native *.d.cma *.cmxa

ctags: $(CTAGSFILES)
	rm -f tags && ctags $(CTAGSFILES)
