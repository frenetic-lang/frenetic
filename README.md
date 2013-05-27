Building from Source
====================

Prerequisites
-------------

- OCaml 4.0 or higher <http://caml.inria.fr/download.en.html>

- OPAM <http://opam.ocamlpro.com>

- The following OCaml libraries:

  - findlib
  - lwt
  - cstruct 
  - oUnit

  These are available on OPAM:

  ```
  $ opam install ocamlfind cstruct lwt ounit
  ```

  If this is your first time using OPAM, you should place the following in your .bashrc file to set up your environment:

  ```
  eval `opam config env`
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
