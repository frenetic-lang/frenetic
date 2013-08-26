Frenetic
========

The [**Frenetic Tutorial**][tutorial] is now available.

Building from Source
--------------------

Prerequisites:

- OCaml 4.0 or higher <http://caml.inria.fr/download.en.html>
- OPAM <http://opam.ocamlpro.com>
- The following OCaml libraries, available from OPAM:
  - cstruct 
  - lwt
  - ocamlfind
  - openflow
  - oUnit
  - pa_ounit

  Instal them using the following command:

  ```
  $ opam install cstruct lwt ocamlfind ounit pa_ounit
  ```

- The following OCaml libraries that we recommend you compile from source:
  - [ocaml-packet][]
  - [ocaml-openflow][]

Building:

From the root directory of the repository, simply run `make`

  ```
  $ make
  ```

Documentation
-------------

We have developed several guides for getting up to speed on programming
OpenFlow networks in general and using Frenetic in particular. See the
[Frenetic Tutorial][tutorial] launch page for more details.

While the Frenetic language is primarily documented in the tutorial, the
[Frenetic Manual][manual] provides a lightweight reference for the Frenetic
DSL. The [API documentation][documentation] is also available online.

[tutorial]: https://github.com/frenetic-lang/frenetic/wiki/Frenetic-Tutorial
[manual]: https://github.com/frenetic-lang/frenetic/wiki/A-NCManual
[documentation]: http://frenetic-lang.github.io/frenetic/docs/index.html
[ocaml-packet]: https://github.com/frenetic-lang/ocaml-packet
[ocaml-openflow]: https://github.com/frenetic-lang/ocaml-openflow
