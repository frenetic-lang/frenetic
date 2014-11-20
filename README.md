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

If you want to use the latest development version of this library, you can pin
this directory in OPAM:

    $ opam pin add packet .

If you make changes to the code, run this command to rebuild the library
and any other libraries that depend on it (e.g., the `openflow` library):

    $ opam install packet

If you add/remove any dependencies, be sure to specify them in the `opam` file.

License
-------

LGPLv3, see LICENSE file for its text.
