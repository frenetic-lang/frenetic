.PHONY: all debug native uninstall install doc clean

LIBNAME = NetCoreLib

MLIFILES = _build/NetCore_Types.mli \
           _build/NetCore_Parser.mli \
           _build/NetCore_Compiler.mli \
           _build/NetCore_Controller.mli \
           _build/NetCore_Stream.mli \
	   _build/NetCore_MacLearning.mli

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
