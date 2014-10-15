all: build

# Implies --enable-quickcheck
ASYNC ?= $(shell if ocamlfind query async >/dev/null 2>&1; then echo --enable-async; else echo --disable-async; fi)
TESTS ?= $(shell if ocamlfind query quickcheck >/dev/null 2>&1; then echo --enable-tests; else echo --disable-tests; fi)

NAME=netkat
J=4

setup.ml: _oasis
	oasis setup
	sed -i 's/archive(syntax, preprocessor) = "syntax.cma"/archive(syntax, preprocessor) = "ulexing.cma syntax.cma"/g' lib/META
	sed -i 's/archive(syntax, preprocessor, native) = "syntax.cmxa"/archive(syntax, preprocessor, native) = "ulexing.cmxa syntax.cmxa"/g' lib/META

setup.data: setup.ml
	ocaml setup.ml -configure $(ASYNC) $(TESTS)

build: setup.data setup.ml
	ocaml setup.ml -build -j $(J)

install: setup.data setup.ml
	ocaml setup.ml -install

test: setup.ml build
	_build/test/Test.byte inline-test-runner netkat -verbose

reinstall: setup.ml
	ocamlfind remove $(NAME) || true
	ocaml setup.ml -reinstall

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log

distclean:
	ocaml setup.ml -distclean
	rm -f setup.data setup.log
