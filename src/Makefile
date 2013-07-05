.PHONY: all clean ctags

all:
	cd openflow && $(MAKE)
	cd ox && $(MAKE)
	cd frenetic && $(MAKE)

clean:
	cd openflow && $(MAKE) clean
	cd ox && $(MAKE) clean
	cd frenetic && $(MAKE) clean

ctags: $(CTAGS_FILES)
	@which ctags 2> /dev/null > /dev/null \
    && rm -f tags && ctags $(CTAGS_FILES) \
    || true
