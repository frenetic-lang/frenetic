.PHONY: all debug native doc library install uninstall clean reinstall

OCAMLBUILD = ocamlbuild -use-ocamlfind

# Note that these are only the .mli files at the top-level.
MLIFILES = $(shell ls _build/*.mli)
CMIFILES = $(shell ls _build/*.cmi)

all: debug doc newuninstall newinstall ctags

debug:
	$(OCAMLBUILD) -cflag -ppopt -cflag -lwt-debug debug.otarget

native:
	$(OCAMLBUILD) native.otarget

verify:
	$(OCAMLBUILD) verify.otarget

doc:
	$(OCAMLBUILD) doc.docdir/index.html

clean:
	rm -rf _build

library: 
	$(OCAMLBUILD) library.otarget

newinstall: debuginstall

debuginstall:
	@cp META_debug META
	ocamlfind install NetCoreLib \
		META $(MLIFILES) $(CMIFILES) NetCoreLib.d.cma
	@rm META

nativeinstall:
	@cp META_native META
	ocamlfind install NetCoreLib \
		META $(MLIFILES) $(CMIFILES) NetCoreLib.cma
	@rm META

newuninstall:
	ocamlfind remove NetCoreLib

install: library 
	ocamlfind install desmoines desmoines_lib/META *.cmi *.cma 

uninstall:
	ocamlfind remove desmoines

reinstall: library
	ocamlfind remove desmoines
	ocamlfind install desmoines desmoines_lib/META _build/*/*.cmi _build/*/*.cma _build/*.cmi _build/*.cma

ctags: $(shell ls *.mli) $(shell ls *.ml)
	ctags test 2>/dev/null >/dev/null && rm -f tags && ctags *.mli *.ml

