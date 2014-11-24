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

**Note** that this workflow assumes that you have OPAM 1.2 installed.

To build and install from source, clone this repository and run the following
command from its root directory:

    opam pin add packet .

If you make changes to the code, run this command to rebuild the library and
any other libraries that depend on it (e.g., the
[openflow][http://github.com/frenetic-lang/ocaml-openflow] library):

    opam install packet

When you add or remove dependencies, be sure to update both the `_oasis` and
`opam` files.

License
-------

LGPLv3, see LICENSE file for its text.
