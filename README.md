OCaml OpenFlow
========

This library supports almost all of OpenFlow 1.0 and also has some experimental support for OpenFlow 1.3.

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

- The [ocaml-packet](https://github.com/frenetic-lang/ocaml-packet) library.
  It is recommended that you build this from source as well.

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

