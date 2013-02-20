all:
	cd coq && $(MAKE)
	cd ocaml && $(MAKE)

test:
	cd ocaml && $(MAKE) test

clean:
	cd coq && $(MAKE) clean
	cd ocaml && $(MAKE) clean
