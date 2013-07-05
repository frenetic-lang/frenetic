.PHONY: reinstall clean configure build

OCAML=ocaml

reinstall: build
	$(OCAML) setup.ml -reinstall

configure:	
	$(OCAML) setup.ml -configure --disable-installexec --disable-docs

build: configure
	$(OCAML) setup.ml -build

clean:
	$(OCAML) setup.ml -clean
