all:
	cd coq && $(MAKE)
	cd ocaml && $(MAKE)

test:
	cd ocaml && $(MAKE)

clean:
	cd coq && $(MAKE) clean
	cd ocaml && $(MAKE) clean
