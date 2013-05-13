all:
	cd ocaml && $(MAKE)

test:
	cd ocaml && $(MAKE) test

clean:
	cd ocaml && $(MAKE) clean
