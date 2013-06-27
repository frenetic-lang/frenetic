.PHONY: all reinstall clean

OCAML=ocaml

reinstall: all
	$(OCAML) setup.ml -reinstall

all:
	$(OCAML) setup.ml -configure --disable-installexec
	$(OCAML) setup.ml -build
	sudo ln -sf `pwd`/frenetic.byte /usr/local/bin/frenetic

clean:
	$(OCAML) setup.ml -clean
