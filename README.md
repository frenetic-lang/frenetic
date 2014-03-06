OCaml OpenFlow
==============

A serialization and protocol library for OpenFlow.

This library supports almost all of OpenFlow 1.0 and also has some experimental
support for OpenFlow 1.3.

[![Build Status](https://travis-ci.org/frenetic-lang/ocaml-openflow.png)](https://travis-ci.org/frenetic-lang/ocaml-openflow)


Installation
------------

You can install the latest release from [OPAM][http://opam.ocamlpro.com/] using
the following command:

    $ opam install openflow

To install the async subpackage, simply ensure that async is installed before
or after installing the library:

    $ opam install async

Development
-----------

To build from source, first ensure that you've installed all dependencies,
which are listed in the `_oasis` file under the openflow, async, and quickcheck
`Library` sections. Then, run the following commands:

    ./configure --enable-tests --enable-quickcheck --enable-async
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
