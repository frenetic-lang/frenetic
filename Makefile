all: build

NAME=topology
J=4

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure

build: setup.data setup.ml
	ocaml setup.ml -build -j $(J)

install: setup.data setup.ml
	ocaml setup.ml -install

test:
	@echo "Note that there are no tests..."

reinstall: setup.ml
	ocamlfind remove $(NAME) || true
	ocaml setup.ml -reinstall

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log