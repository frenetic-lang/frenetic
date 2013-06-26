.PHONY: all reinstall clean

OCAML=ocaml

reinstall: all
	$(OCAML) setup.ml -reinstall

all:
	$(OCAML) setup.ml -configure --disable-frenetic
	$(OCAML) setup.ml -build

clean:
	$(OCAML) setup.ml -clean
