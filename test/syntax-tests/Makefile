all:
	ocamlbuild -use-ocamlfind -syntax camlp4o -pkg netkat.syntax \
		-pkg pa_ounit.syntax -pkg oUnit -pkg netkat -tag thread -pkg core \
		-pkg pa_ounit Main.d.byte

test:
	./Main.d.byte inline-test-runner netkat

clean:
	rm -rf _build
