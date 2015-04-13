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

Development
-----------

To build and install from source, clone this repository and run the following
command from its root directory:

    opam pin add openflow .


If you make changes to the code, run this command to rebuild the library and
any other libraries that depend on it (e.g., the
[frenetic][http://github.com/frenetic-lang/frenetic] library):

    opam install openflow

When you add or remove dependencies, be sure to specify them both in the
`_oasis` and `opam` file.

License
-------

LGPLv3, see LICENSE file for its text.
