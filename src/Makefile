.PHONY: all clean ctags

LIBS = PacketLib \
       OpenFlowLib \
       NetCoreLib 

APPS = NetCore

all: 
	$(foreach LIB,$(LIBS),make -C $(LIB);)
	$(foreach APP,$(APPS),make -C $(APP);)

clean:
	$(foreach LIB,$(LIBS),make -C $(LIB) clean;)
	$(foreach APP,$(APPS),make -C $(APP) clean;)

ctags: $(CTAGSFILES)
	@echo "[5m[31m[1m*** Making ctags is broken! ***[0m"
	rm -f tags && ctags $(CTAGSFILES)
