Frenetic Wrap-up
=================

In this tutorial, you learned to program software-defined networks
using Ox and NetCoreDSL:

  - *Ox* is our low-level platform for implementing OpenFlow controllers,
developed in OCaml.  You saw how to use it to analyze packets on 
an SDN controller, install rules in the data plane and monitor traffic
statistics.  

  - *NetCore* is a high-level domain-specific language for specifying SDN
policies.  In just a few, simple lines of code, you could specify routing 
policy and queries for multi-switch networks in a modular, compositional 
fashion.  The NetCore compiler (built using Ox, of course) compiled your 
high-level programs in to flow tables that are installed automatically in 
the data plane.

Still, there's a lot more to the Frenetic environment than what you
have seen in this tutorial.  One way to get started finding out more is
to dig further in to the code.  For instance, you might look at
[NetCoreLib](https://github.com/frenetic-lang/frenetic/tree/master/src/NetCoreLib), which is the primary library that implements NetCoreDSL.  Take a look at the internal syntax of NetCore in the [NetCoreTypes Module](https://github.com/frenetic-lang/frenetic/blob/master/src/NetCoreLib/NetCore_Types.mli) and then move on to other libraries, including those that implement [Mac Learning](https://github.com/frenetic-lang/frenetic/blob/master/src/NetCoreLib/NetCore_MacLearning.mli) and
[Nat](https://github.com/frenetic-lang/frenetic/blob/master/src/NetCoreLib/NetCore_NAT.ml).  The latter components will introduce you to the basics of
how to construct your own dynamic policies in the Frenetic programming 
environment using NetCoreLib combined with OCaml's [Lwt Threading Libraries](http://ocsigen.org/lwt/manual).

Have fun!

--------------------

![Frenetic.][frenetic_logo]

[frenetic_logo]: images/frenetic-logo.png "Frenetic"