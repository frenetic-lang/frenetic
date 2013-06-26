.PHONY: world configure build clean install uninstall reinstall ctags

OCAML=ocaml

world: configure build

configure:
	$(OCAML) setup.ml -configure --enable-alltargets

build:
	$(OCAML) setup.ml -build

install:
	$(OCAML) setup.ml -install

uninstall:
	$(OCAML) setup.ml -uninstall

reinstall: world
	$(OCAML) setup.ml -reinstall
	sudo ln -f -s `pwd`/Frenetic.byte /usr/local/bin/frenetic

clean:
	$(OCAML) setup.ml -clean

ctags: $(CTAGS_FILES)
	@which ctags 2> /dev/null > /dev/null \
    && rm -f tags && ctags $(CTAGS_FILES) \
    || true
