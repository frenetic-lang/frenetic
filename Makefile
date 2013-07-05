.PHONY: reinstall clean build configure

OCAML=ocaml

reinstall: build
	$(OCAML) setup.ml -reinstall

build: configure
	$(OCAML) setup.ml -build

configure:
	$(OCAML) setup.ml -configure --disable-docs

clean:
	$(OCAML) setup.ml -clean
