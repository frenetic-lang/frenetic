# Frenetic

An open-source SDN controller platform.

[![Build Status](https://travis-ci.org/frenetic-lang/frenetic.png)](https://travis-ci.org/frenetic-lang/frenetic)

## Installation

You can install the latest release from [OPAM](http://opam.ocamlpro.com/) using
the following command:

    $ opam install frenetic

This installs the Frenetic libraries (using `ocamlfind`) and
the `frenetic` executable (typically at `~/.opam/system/bin/frentic`).

## Frenetic HTTP Controller

Instead of using OCaml, you can interact with Frenetic over HTTP.
The following command starts the Frenetic OpenFlow controller on port 6633
and listens for HTTP requests on port 9000:

    $ frenetic http-controller

For example, the following require updates the policy to send
traffic between ports 1 and 2:

    $ curl  -X POST localhost:9000/myapp/update \
        --data "filter (port = 1); port := 2 | filter (port = 2); port := 1"

The following requests blocks until a network event occurs (switch/port
up, packet received, etc.)

    $ curl -X GET localhost:9000/myapp/event

Several applications can connect to the controller simultaneously. But, the user
muust ensure they all have different names. E.g., all the requests above use
`myapp` as the name. All applications have separate event queues and can update
their policies independently. In this version, the policies of all applications
are combined using Frenetic's union operator.

The HTTP interface supports several other messages. E.g., it can be used
to query statistics and update the policy in a JSON format.

## Programming in Python

We have developed Python bindings for Frenetic that use the HTTP interface
described above. You can install them using pip:

    $ pip install frenetic

To run a Python application, you need to start the Frenetic controller first:

    $ frenetic http-controller

There are several examples in this directory:

  https://github.com/frenetic-lang/frenetic/blob/webkat2/lang/python/frenetic/examples/repeater.py

These examples are included with the `pip` package:

    $ python -m frenetic.examples.learning

## Using Other Controllers

If you want to use the Frenetic policy language with a different controller,
you can use the Frenetic compile-server:

    $ frenetic compile-server

In this mode, Frenetic does not run its controller, but makes its compiler
available over HTTP. The compiler accepts Frenetic policies in JSON format
and produces flow tables as JSON strings. For example, using the Python
bindings:

    >>> from frenetic.syntax import *
    >>> import urllib2
    >>> import json
    >>> compiler_url = "http://localhost:9000/compile"
    >>> pol = Filter(Test(Location(Physical(1)))) >> Mod(Location(Physical(2)))
    >>> pol_str = json.dumps(pol.to_json())
    >>> tbls_json = json.loads(urllib2.urlopen(compiler_url, pol_str).read())

## Programming in OCaml

Here's how to create a controller that will do simple forwarding for a tree
topology with fanout 2 and depth 2:

```ocaml
open Core.Std
open Async.Std

let main () =
  let static = Async_NetKAT.Policy create_from_file "examples/tree-2-2.kat" in
  ignore(Async_NetKAT_Controller.start static ()) in

never_returns (Scheduler.go_main ~main ())
```

The static policy can be found in `examples/tree-2-2.kat`. Use the following
command to run mininet with the topology for this example:

    $ sudo mn --controller=remote --mac --topo=tree,2,2
