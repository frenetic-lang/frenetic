OCaml Topology
==============

A library for working with network topologies.

[![Build Status](https://travis-ci.org/frenetic-lang/ocaml-topology.png)](https://travis-ci.org/frenetic-lang/ocaml-topology)

Installation
------------

You can install the latest release from [OPAM][http://opam.ocamlpro.com/] using
the following command:

    $ opam install topology

To build from source, first ensure that you've installed all dependencies,
which are listed in the `_oasis` file under the `Library topology` section.
Then, run the following commands:

    $ make
    $ make install

While developing, you may want to install your latest changes for testing with
other packages. `make install` will fail when trying to reinstall, so use this
command instead:

    $ make reinstall

License
-------

LGPLv3, see LICENSE file for its text.
