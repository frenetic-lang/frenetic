.PHONY: all clean libs apps doc ctags

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
	$(OCAMLBUILD) doc.docdir/index.html

clean:
	$(foreach LIB,$(LIBS),make -C $(LIB) clean;)

ctags: $(CTAGSFILES)
	@echo "[5m[31m[1m*** Making ctags is broken! ***[0m"
	rm -f tags && ctags $(CTAGSFILES)
