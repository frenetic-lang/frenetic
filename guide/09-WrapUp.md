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
to dig further in to implementation.  For instance, you might look at
[NetCoreLib](https://github.com/frenetic-lang/frenetic/tree/master/src/NetCoreLib), which is the primary library that implements NetCoreDSL.  You can examine
the types that implement the internal syntax of NetCore in the [NetCoreTypes Module](https://github.com/frenetic-lang/frenetic/blob/master/src/NetCoreLib/NetCore_Types.mli) and then move on to other libraries, including those that implement [Mac Learning](https://github.com/frenetic-lang/frenetic/blob/master/src/NetCoreLib/NetCore_MacLearning.mli) and
[Nat](https://github.com/frenetic-lang/frenetic/blob/master/src/NetCoreLib/NetCore_NAT.ml).  The latter components will introduce you to the basics of
how to construct your own dynamic policies in the Frenetic programming 
environment using NetCoreLib combined with OCaml's [Lwt Threading Libraries](http://ocsigen.org/lwt/manual).

We have also written a number of papers that explain the theory, design and
implementation of Frenetic in greater depth.  A collection of all our
key papers can be found, together with bibiographic information on the
[Frenetic Publications page](https://frenetic-lang.org/publications.php).  However, some of the highlights follow.
 
#### [Frenetic: A Network Programming Language](http://frenetic-lang.org/publications/frenetic-icfp11.pdf) 

This paper describes our earliest design ideas.  In particular, it
demonstrates the fact OpenFlow is non-modular and therefore a poor
choice for high-level SDN programmers.  It introduces the idea of
parallel composition (though we didn't call it that at the time) and
shows how to write programs that compose high-level statistics queries
with routing specifications.  We didn't yet know how to compile
NetCore properly so our first implementation (in Python) only
supported *reactive micro-flow* compilation of NetCore policies.  In
other words, the first packet of each new flow would be sent to the
controller for processing.  After processing the packet, the
controller would install a single exact-match (microflow) rule on a
switch to process further packets of that sort in the data plane.

#### [A Compiler and Run-time System for Network Programming Languages](http://frenetic-lang.org/publications/compiler-popl12.pdf)

If you are well-versed in programming language theory, you may be
interested in this paper.  It describes the formal semantics of (a
cut-down version of) NetCore for the first time, explains how to
compile it *proactively* (i.e., ahead of time) to flow tables,
somewhat like we currently do, and proved our algorithms correct.
However, we hadn't yet discovered some key ideas, like *sequential
composition*, so the compilation story doesn't end with this paper.
We are hoping to write another paper soon that finishes the story.
Stay tuned.

#### [Abstractions for Network Update](http://frenetic-lang.org/publications/network-update-sigcomm12.pdf)

When implementing dynamic NetCore or Frenetic controllers, a key problem is
how to update a network switches from one policy to the next in a 
semantically meaningful way --- a way that allows programmers to reason
precisely about key properties of their networks such as access control and 
connectivity.  In this paper, we developed new abstract operations
called *consistent network updates* to do just that.  When updating from
global network policy A to global network policy B, a *per-packet* 
consistent update guarantees that every packet will follow either policy
A, or policy B, in its entirety, not some mixture of the two.  Consequently,
if both A and B have no loops or satisfy some key access control property, the 
programmer is guaranteed that no packet will ever follow a loop or break
the access control property, even in the midst of a transition from one
policy to the next.  A *per-flow* consistent update generalizes this idea
by ensuring that *sets* of packets (entire flows) follow the same policy,
be it A or B.  After defining the abstractions, we show to implement them 
efficiently, prove that our implementation is correct and exploit the semantics
to support automated verification of connectivity constraints with the 
NuSMV model checker.

#### [Composing Software Defined Networks](http://frenetic-lang.org/publications/composing-nsdi13.pdf)

In this paper, we further explored the design of high-level languages
for SDN programming.  In particular, it was here that we really
started to think of SDN policies as functions, and once we did so, it
was clear that we needed a general form of "function composition" in
our designs.  We now call that form of function composition
*sequential composition*, and as you have seen in this tutorial, it is
an important device for defining modular policies.  Another key
feature of this paper is a series of techniques used to construct
abstract, virtual networks.  For instance, we show how to present an
SDN application-level programmer with the illusion that they are
programming on just "one big switch" instead of on a complex network
made out of many intricately interconnected switches.  Interesting,
sequential composition, as well as some new features such as virtual
packet fields, were key ingredients in supporting these abstractions.

#### [Machine-Verified Network Controllers](http://frenetic-lang.org/publications/verified-pldi13.pdf)

*TO DO: Mark, Arjun?*

### Conclusions

Overall, the Frenetic project aims to develop new high-level abstractions
that make programming networks simpler, more reliable and more secure.
We have made some progress on this front by blending the skills of 
both networking and programming languages researchers.  And it's 
taken a great [team of people](https://frenetic-lang.org/members.php) from 
undergraduate students to graduate
students to post docs and faculty to put it together.

### Acknowledgement

This work is supported in part by ONR grants N00014-09-1-0770 and
N00014-09-1-0652, NSF grants CNS-1111698 and CNS-1111520, TRUST, and
gifts from Dell, Intel, and Google. Any opinions, ﬁndings, and
recommendations are those of the authors and do not necessarily reﬂect
the views of the ONR or NSF.

![Frenetic.][frenetic_logo]

[frenetic_logo]: images/frenetic-logo.png "Frenetic"