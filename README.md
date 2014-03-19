# Frenetic

An open-source SDN controller platform.

[![Build Status](https://travis-ci.org/frenetic-lang/frenetic.png)](https://travis-ci.org/frenetic-lang/frenetic)

## Installation

You can install the latest release from [OPAM](http://opam.ocamlpro.com/) using
the following command:

    $ opam install frenetic

To install the async subpackage, simply ensure that async is installed before
or after installing the library:

    $ opam install async

## Development

To build from source, first ensure that you've installed all dependencies,
which are listed in the `_oasis` file under the netkat, async, and quickcheck
`Library` sections. To install dependencies that are part of the Frenetic
project, you may want to install a custom OPAM repository maintained that the
Frenetic project maintains.

    $ opam repository add frenetic https://github.com/frenetic-lang/opam-bleeding.git
    $ opam update

Once that's done, packages that are part of the Frenetic project will be
installed from the HEAD of their master branch on github. Install those
packages and then build and install Frenetic using the following commands:

    ./configure --enable-tests --enable-quickcheck --enable-async
    $ make
    $ make test
    $ make install

## Usage Example

Here's how to create a controller that will do simple forwarding for a tree
topology with fanout 2 and depth 2:

```ocaml
open Core.Std
open Async.Std

let main () =
  let static = Async_NetKAT.create_from_file "examples/tree-2-2.kat" in
  Async_NetKAT_Controller.start static () in

never_returns (Scheduler.go_main ~main ())
```

The static policy can be found in `examples/tree-2-2.kat`. Use the following
command to run mininet with the topology for this example:

    $ sudo mn --controller=remote --mac --topo=tree,2,2

## License

LGPLv3, see LICENSE file for its text.
