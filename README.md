OCaml Topology
==============

A library for working with network topologies.

[![Build Status](https://travis-ci.org/frenetic-lang/ocaml-topology.png)](https://travis-ci.org/frenetic-lang/ocaml-topology)

Installation
------------

You can install the latest release from [OPAM](http://opam.ocamlpro.com/) using
the following command:

    opam install topology

Development
-----------

To build and install from source, clone this repository and run the following
command from its root directory:

    opam pin add topology .

If you make changes to the code, run this command to rebuild the library and
any other libraries that depend on it (e.g., the
[frenetic][http://github.com/frenetic-lang/frenetic] library):

    opam install topology

When you add or remove dependencies, be sure to specify them both in the
`_oasis` and `opam` file.

License
-------

LGPLv3, see LICENSE file for its text.
