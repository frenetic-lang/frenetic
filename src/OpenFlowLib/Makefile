.PHONY: all debug native uninstall install doc clean

LIBNAME = OpenFlowLib

MLIFILES = _build/OpenFlow0x01.mli \
           _build/OpenFlow0x01_Parser.mli \
           _build/OpenFlow0x01_Platform.mli \
           _build/OpenFlow0x01_TestPlatform.mli

CMIFILES = $(MLIFILES:.mli=.cmi)

OCAMLBUILD = ocamlbuild -use-ocamlfind

all: debug native uninstall install 

debug:
	$(OCAMLBUILD) -cflag -ppopt -cflag -lwt-debug debug.otarget

native:
	$(OCAMLBUILD) native.otarget

uninstall:
	ocamlfind remove $(LIBNAME)

install:
	ocamlfind install $(LIBNAME) META $(MLIFILES) $(CMIFILES) $(LIBNAME).d.cma $(LIBNAME).cmxa $(LIBNAME).a

doc:
	$(OCAMLBUILD) doc.docdir/index.html

clean:
	rm -rf _build *.d.byte *.native *.d.cma *.cmxa
