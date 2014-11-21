OCaml OpenFlow
==============

A serialization and protocol library for OpenFlow.

This library supports almost all of OpenFlow 1.0 and also has some experimental
support for OpenFlow 1.3.

[![Build Status](https://travis-ci.org/frenetic-lang/ocaml-openflow.png)](https://travis-ci.org/frenetic-lang/ocaml-openflow)


Installation
------------

You can install the latest release from [OPAM](http://opam.ocamlpro.com/) using
the following command:

    $ opam install openflow

To install the async subpackage, simply ensure that async is installed before
or after installing the library:

    $ opam install async

Development
-----------


If you want to use the latest development version of this library, you can pin
this directory in OPAM:

    $ opam pin add openflow .

If you make changes to the code, run this command to rebuild the library
and any other libraries that depend on it (e.g., the `frenetic` library):

    $ opam install openflow

If you add/remove any dependencies, be sure to specify them in the `opam` file.

License
-------

LGPLv3, see LICENSE file for its text.
