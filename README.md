OCaml Packet
========

This library includes serializers for ethernet, TCP, IP, ARP, ICMP, and others.

Building from Source
--------------------

Prerequisites:

- OCaml 4.0 or higher <http://caml.inria.fr/download.en.html>

- OPAM <http://opam.ocamlpro.com>

- The following OCaml libraries:

  - ocamlfind
  - lwt
  - cstruct 
  - oUnit

  These are available on OPAM:

  ```
  $ opam install ocamlfind cstruct lwt ounit
  ```

Building:

From the root directory of the repository, simply run `make`

  ```
  $ make
  ```

Installing:

Run `make install` from the root directory of the repository.

  ```
  $ make install
  ```

Testing:

Enable and run tests from the root directory of the repository:

  ```
  $ ocaml setup.ml -configure --enable-quickcheck --enable-tests
  ...
  $ make test
  ```
