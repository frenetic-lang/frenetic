OCaml Packet
============

A serialization library for network packets.

[![Build Status](https://travis-ci.org/frenetic-lang/ocaml-packet.png)](https://travis-ci.org/frenetic-lang/ocaml-packet)

Installation
------------

You can install the latest release from [OPAM][http://opam.ocamlpro.com/] using
the following command:

    $ opam install packet

Development
-----------

To build from source, first ensure that you've installed all dependencies,
which are listed in the `_oasis` file under the `Library packet` and `Library
quickcheck` sections. Then, run the following commands:

    ./configure --enable-tests --enable-quickcheck
    $ make
    $ make test
    $ make install

While developing, you may want to install your latest changes for testing with
other packages. `make install` will fail when trying to reinstall, so use this
command instead:

    $ make reinstall

License
-------

LGPLv3, see LICENSE file for its text.
