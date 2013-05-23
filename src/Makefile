APPLICATIONS = NetCore/Main \
               Test/Test

LIBRARIES = FreneticLib \
            PacketLib \
            OpenFlowLib \
            NetCoreLib 

SUFFIXES = .a .cmxa .cma .d.cma

.PHONY: all native byte debug install uninstall doc clean $(LIBRARIES)

OCAMLBUILD = ocamlbuild -use-ocamlfind $(addprefix -I ,$(LIBRARIES))

OCAMLFIND = ocamlfind

all: native byte debug $(LIBRARIES)

native: $(addsuffix .native, $(APPLICATIONS))

byte: $(addsuffix .byte, $(APPLICATIONS))

debug: $(addsuffix .d.byte, $(APPLICATIONS))

%.native %.byte:
	$(OCAMLBUILD) $@

%.a %.cmxa %.cma:
	$(OCAMLBUILD) -no-links $@

%.d.byte:
	$(OCAMLBUILD) -cflag -ppopt -cflag -lwt-debug $@

%.d.cma:
	$(OCAMLBUILD) -no-links -cflag -ppopt -cflag -lwt-debug $@

$(LIBRARIES):
	$(foreach SUFFIX, $(LIBRARY_SUFFIXES), \
            $(OCAMLBUILD) $(addsuffix $(SUFFIX), $@/$@))

install: $(LIBRARIES)
	$(foreach LIBRARY,$(LIBRARIES), \
            $(OCAMLFIND) install $(LIBRARY) $(LIBRARY)/META \
                $(foreach FILE, $(shell cat $(LIBRARY)/MANIFEST), \
                    _buildb/$(LIBRARY)/$(FILE).mli _build/$(LIBRARY)/$(FILE).cmi) \
                $(foreach SUFFIX, $(SUFFIXES), \
                    $(addsuffix $(SUFFIX), _build/$(LIBRARY)/$(LIBRARY)));)

uninstall: 
	$(foreach LIBRARY,$(LIBRARIES),$(OCAMLFIND) remove $(LIBRARY);)

doc:
	$(OCAMLBUILD) doc.docdir/index.html

clean:
	$(OCAMLBUILD) -clean

ctags: $(CTAGSFILES)
	@echo "[5m[31m[1m*** Making ctags is broken! ***[0m"
	rm -f tags && ctags $(CTAGSFILES)
