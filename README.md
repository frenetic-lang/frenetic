Frenetic
========

Work in Progress: Frenetic will be ready by June 10th, along with a
standalone tutorial.

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

Documentation
-------------

The API documentation is available
[online](http://htmlpreview.github.io/?https://github.com/frenetic-lang/frenetic/blob/master/doc/index.html).

Hacking Wisdom
--------------

If you're using the user-mode reference switch, emit CONTROLLER actions last.
