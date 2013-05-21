.PHONY: all debug native uninstall install doc clean

LIBNAME = PacketLib

MLIFILES = _build/Packet.mli \
           _build/Packet_Parser.mli

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
	ocamlfind install $(LIBNAME) META $(MLIFILES) $(CMIFILES) $(LIBNAME).d.cma $(LIBNAME).a $(LIBNAME).cmxa

doc:
	$(OCAMLBUILD) doc.docdir/index.html

clean:
	rm -rf _build *.d.byte *.native *.d.cma *.cmxa
