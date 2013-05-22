.PHONY: all clean libs apps doc ctags

OCAMLBUILD = ocamlbuild -use-ocamlfind

LIBS = FreneticLib \
       PacketLib \
       OpenFlowLib \
       NetCoreLib 

APPS = NetCore

.PHONY: $(LIBS) $(APPS)

all: libs apps

libs: $(LIBS)

apps: $(APPS)

$(LIBS):
	$(MAKE) -C $@

$(APPS): 
	$(MAKE) -C $@

doc:
	$(OCAMLBUILD) -no-hygiene doc.docdir/index.html

clean:
	$(foreach LIB,$(LIBS),make -C $(LIB) clean;)
	$(OCAMLBUILD) -clean

ctags: $(CTAGSFILES)
	@echo "[5m[31m[1m*** Making ctags is broken! ***[0m"
	rm -f tags && ctags $(CTAGSFILES)
