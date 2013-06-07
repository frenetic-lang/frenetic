Frenetic Wrap-up
=================

In this tutorial, you learned to program software-defined networks
using Ox and NetCore.  Ox is a low-level platform for implementing
OpenFlow controllers.  You saw how to use it to  analyze traffic on 
an SDN controller, install rules in the data plane and monitor traffic
volume.  When studying NetCore, you were exposed to a high-level 
domain-specific language for managing SDN.  In just a few lines of
code, you could specify routing policy and queries for multi-switch networks
in a modular, compositional fashion.  The NetCore compiler (built using Ox,
of course) compiled your high-level programs in to flow tables that are
installed automatically in the data plane.

Still, there's a lot more to the Frenetic environment than what you
have seen in this tutorial.  One way to get started finding out more is
to dig further in to implementation.  You might start by looking at
(NetCoreLib)[https://github.com/frenetic-lang/frenetic/tree/master/src/NetCoreLib] ...

We have also written a number of papers explaining various implementation
and design ideas in greater detail.

Annotated bib of papers.

Frenetic logo.