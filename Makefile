INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

build:
	time -p jbuilder build @install

install: build
	jbuilder install $(INSTALL_ARGS)

uninstall:
	jbuilder uninstall $(INSTALL_ARGS)

reinstall: uninstall install

clean:
	jbuilder clean

doc:
	jbuilder build @doc

test:
	jbuilder build @runtest

all: build test doc

utop: install
	utop-full -short-paths -init ocamlinit

.PHONY: build install uninstall reinstall clean doc test all utop
