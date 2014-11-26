# Frenetic

An open-source SDN controller platform.

[![Build Status](https://travis-ci.org/frenetic-lang/frenetic.png)](https://travis-ci.org/frenetic-lang/frenetic)

## Installation

You can install the latest release from [OPAM](http://opam.ocamlpro.com/) using
the following command:

    opam install frenetic

To install the async subpackage, simply ensure that async is installed before
or after installing the library:

    opam install async

## Development

To build and install from source, clone this repository and run the following
command from its root directory:

    opam pin add frenetic .

If you make changes to the code, run this command to rebuild the library and
any other libraries that depend on it:

    opam install frenetic

When you add or remove dependencies, be sure to specify them both in the
`_oasis` and `opam` file.

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
