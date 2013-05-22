.PHONY: all clean libs apps ctags

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

clean:
	$(foreach LIB,$(LIBS),make -C $(LIB) clean;)

ctags: $(CTAGSFILES)
	@echo "[5m[31m[1m*** Making ctags is broken! ***[0m"
	rm -f tags && ctags $(CTAGSFILES)
