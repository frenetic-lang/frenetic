Building from Source
====================

Prerequisites
-------------

- Coq 8.4 <http://coq.inria.fr/download>

  Newer versions of Coq often have changes that break older
  programs. We recommend using exactly this version.

- OCaml 4 or higher <http://caml.inria.fr/download.en.html>

- OPAM <http://opam.ocamlpro.com>

- The following OCaml libraries:

  - findlib
  - lwt
  - cstruct version 0.5.3
  - oUnit

  These are available on OPAM:

  ```
  $ opam install ocamlfind
  $ opam install cstruct.0.5.3
  $ opam install lwt
  $ opam install ounit
  ```

Building
--------

- From the root directory of the repository, run `make`

  ```
  $ make
  ```

  Make compiles the Coq code first, extracts it to OCaml, and then compiles
  the OCaml shim.
