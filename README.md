OCaml OpenFlow
==============

A serialization library for OpenFlow, with support for writing Lwt-based
OpenFlow controllers.


This library supports almost all of OpenFlow 1.0 and also has some experimental
support for OpenFlow 1.3.

[![Build Status](https://travis-ci.org/frenetic-lang/ocaml-openflow.png)](https://travis-ci.org/frenetic-lang/ocaml-openflow)


Building from Source
--------------------

If you want to write a lwt-based controller:

    $ opam install openflow lwt

This installed the `openflow.lwt` subpackage.

If you just want serialization support:

    $ opam install openflow


Hacking
-------

The `_oasis` file specifies all the dependencies. If you're hacking on the
library, you should install the dependencies needed to build the main
library, the sub-packages, and the test suite.

    $ opam install ocamlfind cstruct quickcheck ounit pa_ounit lwt
    $ ./configure --enable-tests --enable-quickcheck --enable-lwt

