INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

build:
	time -p dune build @install @all --profile release

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

clean:
	dune clean

doc:
	dune build @doc --profile release

test:
	dune runtest --no-buffer --profile release

all: build test doc

.PHONY: build install uninstall reinstall clean doc test all
