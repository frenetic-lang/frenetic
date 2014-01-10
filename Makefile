all: build

LWT ?= $(shell if ocamlfind query lwt.unix >/dev/null 2>&1; then echo --enable-lwt; else echo --disable-lwt; fi)
ASYNC ?= $(shell if ocamlfind query async >/dev/null 2>&1; then echo --enable-async; else echo --disable-async; fi)
# Implies --enable-quickcheck
TESTS ?= $(shell if ocamlfind query quickcheck >/dev/null 2>&1; then echo --enable-tests; else echo --disable-tests; fi)

NAME=openflow
J=4

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure $(LWT) $(ASYNC) $(TESTS)

build: setup.data setup.ml
	ocaml setup.ml -build -j $(J)

install: setup.data setup.ml
	ocaml setup.ml -install

test: setup.ml build
	_build/test/Test.byte inline-test-runner openflow

reinstall: setup.ml
	ocamlfind remove $(NAME) || true
	ocaml setup.ml -reinstall

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log