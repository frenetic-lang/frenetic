.PHONY: reinstall clean configure build

OCAML=ocaml

reinstall: build
	$(OCAML) setup.ml -reinstall

configure:	
	$(OCAML) setup.ml -configure --disable-installexec --disable-docs

build: configure
	$(OCAML) setup.ml -build
	sudo ln -sf `pwd`/Frenetic.byte /usr/local/bin/frenetic

clean:
	$(OCAML) setup.ml -clean
