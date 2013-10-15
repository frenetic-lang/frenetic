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
  - ocamlgraph
  - openflow
  - oUnit
  - pa_ounit
  - quickcheck

  Instal them using the following command:

  ```
  $ opam install cstruct lwt ocamlfind ocamlgraph ounit pa_ounit quickcheck
  ```

- The following OCaml libraries that we recommend you compile from source:
  - [ocaml-packet][]
  - [ocaml-openflow][]
  - [ocaml-topology][]

Optionally, install Z3 <http://z3.codeplex.com> if you want to use Frenetic's verification tools.
[Z3 binaries] are also available.

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
[ocaml-topology]: https://github.com/frenetic-lang/ocaml-topology
[Z3 binaries]: http://leodemoura.github.io/blog/2013/02/15/precompiled.html
