APPS = NetCore/Main

LIBS = FreneticLib \
       PacketLib \
       OpenFlowLib \
       NetCoreLib 

.PHONY: all native byte debug install uninstall doc clean $(LIBS)

OCAMLBUILD = ocamlbuild -use-ocamlfind -no-links $(addprefix -I ,$(LIBS))

OCAMLFIND = ocamlfind

all: native byte debug $(LIBS)

native: $(addsuffix .native, $(APPS))

byte: $(addsuffix .byte, $(APPS))

debug: $(addsuffix .d.byte, $(APPS))

%.native %.byte %.a %.cmxa %.cma:
	$(OCAMLBUILD) $@

%.d.byte %.d.cma:
	$(OCAMLBUILD) -cflag -ppopt -cflag -lwt-debug $@

$(LIBS):
	$(OCAMLBUILD) $@/$@.a
	$(OCAMLBUILD) $@/$@.cmxa
	$(OCAMLBUILD) $@/$@.cma
	$(OCAMLBUILD) $@/$@.d.cma

install: $(LIBS)
	$(foreach LIB,$(LIBS), \
            $(OCAMLFIND) install $(LIB) $(LIB)/META \
            $(foreach FILE,$(shell cat $(LIB)/MANIFEST),_build/$(LIB)/$(FILE).mli _build/$(LIB)/$(FILE).cmi) \
            $(foreach SUF,.a .cmxa .cma .d.cma, $(addsuffix $(SUF), _build/$(LIB)/$(LIB)));)

uninstall: 
	$(foreach LIB,$(LIBS),$(OCAMLFIND) remove $(LIB);)

doc:
	$(OCAMLBUILD) doc.docdir/index.html

clean:
	$(OCAMLBUILD) -clean

ctags: $(CTAGSFILES)
	@echo "[5m[31m[1m*** Making ctags is broken! ***[0m"
	rm -f tags && ctags $(CTAGSFILES)



# CMIFILES = $(MLIFILES:.mli=.cmi)
# 
# OCAMLBUILD = ocamlbuild -no-links 
# 
# OCAMLFIND = ocamlfind
# 
# all: debug native uninstall install 
# 
# debug:
# 	$(OCAMLBUILD) -cflag -ppopt -cflag -lwt-debug $(LIBNAME).d.cma
# 
# byte:
# 	$(OCAMLBUILD) $(LIBNAME).cma
# 
# native:
# 	$(OCAMLBUILD) $(LIBNAME).cmxa $(LIBNAME).a
# 
# install:
# 	$(OCAMLFIND) install $(LIBNAME) META \
#         $(MLIFILES) \
#         $(CMIFILES) \
#         _build/$(LIBNAME).d.cma \
#         _build/$(LIBNAME).cmxa \
#         _build/$(LIBNAME).a
# 
# uninstall:
# 	$(OCAMLFIND) remove $(LIBNAME)
# 
# clean:
# 	$(OCAMLBUILD) -clean
