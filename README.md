OCaml Packet
============

A serialization library for ethernet, TCP, IP, ARP, ICMP, and others.

This package is available on OPAM:

    $ opam install packet

Hacking
-------

The `_oasis` file specifies all the dependencies. If you're hacking on the
library, you should install the dependencies needed to build the main
library and the test suite.

    $ opam install ocamlfind cstruct quickcheck ounit pa_ounit
    $ ./configure --enable-tests --enable-quickcheck
    $ make test
