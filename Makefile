all: build

J=4

setup.ml: _oasis
	oasis setup
	sed -i 's/archive(syntax, preprocessor) = "syntax.cma"/archive(syntax, preprocessor) = "ulexing.cma syntax.cma"/g' frenetic/lib/META
	sed -i 's/archive(syntax, preprocessor, native) = "syntax.cmxa"/archive(syntax, preprocessor, native) = "ulexing.cmxa syntax.cmxa"/g' frenetic/lib/META

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
