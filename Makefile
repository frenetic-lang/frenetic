INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

build:
	time -p dune build @install

install: build
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall install

clean:
	dune clean

doc:
	dune build @doc

test:
	dune runtest

updatetest:
	dune runtest --auto-promote

all: build test doc

utop: install
	utop-full -short-paths -init ocamlinit

.PHONY: build install uninstall reinstall clean doc test all utop updatetest
