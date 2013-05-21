.PHONY: all debug native uninstall install doc clean

OCAMLBUILD = ocamlbuild -use-ocamlfind

MLIFILES = _build/Packet.mli \
           _build/Packet_Parser.mli

CMIFILES = $(MLIFILES:.mli=.cmi)

all: debug native uninstall install 

debug:
	$(OCAMLBUILD) -cflag -ppopt -cflag -lwt-debug debug.otarget

native:
	$(OCAMLBUILD) native.otarget

uninstall:
	ocamlfind remove PacketLib

install:
	ocamlfind install PacketLib META $(MLIFILES) $(CMIFILES) PacketLib.d.cma PacketLib.cmxa

doc:
	$(OCAMLBUILD) doc.docdir/index.html

clean:
	rm -rf _build *.d.byte *.native *.d.cma *.cmxa
