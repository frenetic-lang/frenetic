APPLICATIONS = Applications/NetCore \
               Applications/Ox \
               Applications/Test \
               Applications/Verify \
               Applications/NetCoreConsistent

LIBRARIES = FreneticLib \
            PacketLib \
            OpenFlowLib \
            NetCoreLib \
            OxLib 

CTAGS_FILES = $(shell ls $(addsuffix /*.ml,$(LIBRARIES)) \
                         $(addsuffix /*.ml,$(LIBRARIES)))

SUFFIXES = .a .cmxa .cma .d.cma

.PHONY: all native byte debug install uninstall doc clean $(LIBRARIES)

OCAMLBUILD = ocamlbuild -use-ocamlfind $(addprefix -I ,$(LIBRARIES))

OCAMLFIND = ocamlfind

all: ctags native byte debug $(LIBRARIES)

native: $(addsuffix .native, $(APPLICATIONS))

byte: $(addsuffix .byte, $(APPLICATIONS))

debug: $(addsuffix .d.byte, $(APPLICATIONS))

%.native %.byte:
	$(OCAMLBUILD) -I Applications $@

%.d.byte:
	$(OCAMLBUILD) -I Applications -cflag -ppopt -cflag -lwt-debug $@

%.a %.cmxa %.cma:
	$(OCAMLBUILD) -I Applications -no-links $@

%.d.cma:
	$(OCAMLBUILD) -I applications -no-links -cflag -ppopt -cflag -lwt-debug $@

$(LIBRARIES):
	$(foreach SUFFIX, $(SUFFIXES), \
	    $(OCAMLBUILD) $(addsuffix $(SUFFIX),$@/$@);)

install: $(LIBRARIES)
	$(foreach LIBRARY,$(LIBRARIES), \
            $(OCAMLFIND) install $(LIBRARY) $(LIBRARY)/META \
                $(foreach FILE, $(shell cat $(LIBRARY)/MANIFEST), \
                    _build/$(LIBRARY)/$(FILE).mli _build/$(LIBRARY)/$(FILE).cmi) \
                $(foreach SUFFIX, $(SUFFIXES), \
                    $(addsuffix $(SUFFIX), _build/$(LIBRARY)/$(LIBRARY)));)

uninstall: 
	$(foreach LIBRARY, $(LIBRARIES), \
            $(OCAMLFIND) remove $(LIBRARY);)

doc:
	$(OCAMLBUILD) doc.docdir/index.html

clean:
	$(OCAMLBUILD) -clean

ctags: $(CTAGS_FILES)
	@which ctags 2> /dev/null > /dev/null \
    && rm -f tags && ctags $(CTAGS_FILES) \
    || true
