Building from Source
====================

Prerequisites
-------------

- OCaml 4 or higher <http://caml.inria.fr/download.en.html>

- OPAM <http://opam.ocamlpro.com>

- The following OCaml libraries:

  - findlib
  - lwt
  - cstruct 
  - oUnit
  - menhir

  These are available on OPAM:

  ```
  $ opam install ocamlfind cstruct lwt ounit menhir
  ```

Building
--------

From the root directory of the repository, simply run `make`

  ```
  $ make
  ```

Hacking Wisdom
==============

If you're using the user-mode reference switch, emit CONTROLLER actions last.
