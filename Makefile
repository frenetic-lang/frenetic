all: build

J=4

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	./configure

build: setup.data setup.ml
	ocaml setup.ml -build -j $(J)

install: setup.data setup.ml
	ocaml setup.ml -install

reinstall: setup.ml
	ocaml setup.ml -reinstall

uninstall: setup.ml
	ocaml setup.ml -uninstall

test: setup.ml build
	ocaml setup.ml -test $(TESTFLAGS)

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log

distclean:
	ocaml setup.ml -distclean
	rm -f setup.data setup.log

doc:
	ocaml setup.ml -doc