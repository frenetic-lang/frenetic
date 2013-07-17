Frenetic
========

The [**Frenetic Tutorial**](https://github.com/frenetic-lang/frenetic/wiki/Frenetic-Tutorial) is now available.

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

- The [ocaml-packet](https://github.com/frenetic-lang/ocaml-packet) and
  [ocaml-openflow](https://github.com/frenetic-lang/ocaml-openflow) libraries.
  It is recommended that you build these from source as well.

Building:

From the root directory of the repository, simply run `make`

  ```
  $ make
  ```

Documentation
-------------

We have developed several guides for getting up to speed on programming OpenFlow networks in general and using Frenetic in particular.  See the [Frenetic Tutorial](https://github.com/frenetic-lang/frenetic/wiki/Frenetic-Tutorial) launch page for more details.

While the Frenetic language is primarily documented in the tutorial, the [Frenetic
Manual](https://github.com/frenetic-lang/frenetic/wiki/A-NCManual) provides a lightweight reference for the Frenetic DSL.
The [API
documentation](http://frenetic-lang.github.io/frenetic/docs/index.html)
is also available online.

