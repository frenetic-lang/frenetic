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

Building:

From the root directory of the repository, simply run `make`

  ```
  $ make
  ```

Documentation
-------------

We have developed several guides for getting up to speed on programming
OpenFlow networks in general and using Frenetic in particular:

* **[Ox](https://github.com/frenetic-lang/frenetic/blob/master/guide/Ox-tutorial.md)**.  This guide provides a high-level overview of software defined networking as well as an introduction to the OpenFlow protocol using Ox, an OCaml-based controller platform for controlling an OpenFlow-enabled network by sending and receiving OpenFlow messages directly.
* **[Frenetic](https://github.com/frenetic-lang/frenetic/blob/master/guide/06-NetCoreIntroduction.md)**.  This guide introduces the design principles of the Frenetic language along with the nuts and bolts of using Frenetic to control an OpenFlow-enabled network.

For readers unfamiliar with programming software defined networks, we recommend
starting with the Ox tutorial before progressing to the Frenetic tutorial.

The Frenetic language is primarily documented in the [Frenetic
Manual](https://github.com/frenetic-lang/frenetic/blob/master/guide/A-NCManual.md).
The [API
documentation](http://htmlpreview.github.io/?https://github.com/frenetic-lang/frenetic/blob/master/doc/index.html)
is also available online.

