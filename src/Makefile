.PHONY: all clean ctags

all:
	cd PacketLib && $(MAKE)
	cd OpenFlowLib && $(MAKE)
	cd OxLib && $(MAKE)
	cd NetCoreLib && $(MAKE)

clean:
	cd PacketLib && $(MAKE) clean
	cd OpenFlowLib && $(MAKE) clean
	cd OxLib && $(MAKE) clean
	cd NetCoreLib && $(MAKE) clean

ctags: $(CTAGS_FILES)
	@which ctags 2> /dev/null > /dev/null \
    && rm -f tags && ctags $(CTAGS_FILES) \
    || true
