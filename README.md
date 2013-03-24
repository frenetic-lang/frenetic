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
  - cstruct 
  - oUnit
  - menhir

  These are available on OPAM:

  ```
  $ opam install ocamlfind cstruct lwt ounit menhir
  ```

Building
--------

- From the root directory of the repository, run `make`

  ```
  $ make
  ```

  Make compiles the Coq code first, extracts it to OCaml, and then compiles
  the OCaml shim.

Hacking
=======

Coq Wisdom
----------

Do not use type-classes in Coq code that is meant to be extracted to OCaml.
Type-classes aren't expressible in OCaml's type system, and Coq happily
extracts it to use `Obj.magic` and other nonsense.

However, type-classes can help structure proofs. So, very carefully split
factor the Coq into code and proof.

OCaml Wisdom
------------

If you're using the user-mode reference switch, emit CONTROLLER actions last.
