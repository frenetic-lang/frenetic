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

If you want to use the latest development version of this library, you can pin
this directory in OPAM:

    $ opam pin add frenetic .

If you make changes to the code, run this command to rebuild the library:

    $ opam install frenetic

If you have used OPAM to install applications that depend on Frenetic, they
will be recompiled too.

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
