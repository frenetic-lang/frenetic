NetCore Tutorial
=============

The goal of this tutorial is to teach readers how to program a
Software-Defined Network (SDN) running OpenFlow using the *NetCore*
Domain-specific Programming Language (NetCoreDSL).  NetCoreDSL is 
designed primarily to allow users to program static (unchanging)
network configurations.  However, NetCoreDSL also contains a limited set of
building blocks that allow programmers to craft dynamic policies that generate
a series of static NetCore configurations.

This tutorial should also be viewed as a stepping stone towards
learning how to program in the more powerful *Frenetic* environment.
*Frenetic* is a general-purpose SDN programming language embedded as a
set of libraries in OCaml.  In Frenetic, one programs applications
that react to network events such as topology changes and statistics
queries.  On seeing a network event, a Frenetic application will
generate a new static network configuration and pass it off to the
Frenetic run-time system.  The Frenetic run-time system will compile
the static configuraion in to OpenFlow and update the running network
with the compiled policy in a [per-packet consistent
manner](http://frenetic-lang.org/publications/network-update-sigcomm12.pdf).
The static configurations constructed by Frenetic applications are
built using [NetCoreLib](http://frenetic-lang.github.io/frenetic/docs/NetCore_Types.html),
which has the same semantics as the NetCore DSL described in this
tutorial.  Hence, a Frenetic program is really just a general-purpose
OCaml program that reacts to network events and generates a stream of
NetCore policies.  Since NetCoreLib and NetCoreDSL are effectively the
same (with NetCoreDSL simply adding a pleasing domain-specific,
user-level syntax), we will drop the "DSL" part of the name from this
point forward and simply refer to NetCore.

Before divining in to the technical portion of the tutorial, we
briefly motivate the design of NetCore and Frenetic.  The technical
portions of the tutorial focus on explaining the syntax and semantics
of NetCore and illustrating its use on a number of simple examples.
There are also a number of exercises for the reader.  

Getting Started
---------------

As you read this document, we encourage you to experiment with the examples 
and try the exercises.  To do so, you will need to download and start up the
tutorial VM.  Please see the [instructions here](https://github.com/frenetic-lang/frenetic/blob/master/guide/01-Introduction.md#getting-started).

Motivation for the Frenetic and NetCore Designs
-----------------------------------------------

*Much in this section was plagiarized from IEEE overview paper*

*Should this section be cut and just get to the tutorial?*

Traditional networks are built out of special-purpose devices running distributed protocols that provide functionality such as routing, trafﬁc monitoring, load balancing, NATing and access control. These devices have a tightly-integrated control and data plane, and network operators must separately conﬁgure every protocol on each individual device. This configuration task is a challenging one as network operators must struggle with a host of different baroque, low-level, vendor-specific configuration languages.  Moreover, the pace of innovation is slow as device internals and APIs are often private and proprietary, making it difficult to develop new protocols or functionality to suit client needs.    

Recent years, however, have seen growing interest in software-deﬁned networks (SDNs), in which a logically-centralized controller manages the packet-processing functionality of a distributed collection of switches. SDNs make it possible for programmers to control the behavior of the network directly, by conﬁguring the packet-forwarding rules installed on each switch.  Moreover, the Open Networking Foundation is committed to developing a standard, open, vendor-neutral protocol for controlling collections of switches.  This protocol is OpenFlow.

SDNs can both simplify existing applications and also serve as a platform for developing new ones. For example, to implement shortest-path routing, the controller can calculate the forwarding rules for each switch by running Dijkstra’s algorithm on the graph of the network topology instead of using a more complicated distributed protocol. To conserve energy, the controller can selectively shut down links or even whole switches after directing trafﬁc along other paths. To enforce ﬁne-grained access control policies, the controller can consult an external authentication server and install custom firewall rules.

But although SDNs makes it possible to program the network, they do not make it easy. Protocols such as OpenFlow expose an interface that closely matches the features of the underlying switch hardware. Roughly speaking, OpenFlow allows programmers to manually install and uninstall individual packet-processing rules.  First-generation controller systems such as NOX, Beacon, and Floodlight support the same low-level interface, which forces applications to be implemented using programs that manipulate the fine-grained state of individual devices.  Unfortunately, it is extremely difficult to develop independent program components, such as a router, firewall and network monitor, that collaborate to control the flow of traffic through a network since the application must ultimately install a *single* set of low-level rules on the underlying switches.  This single set of rules must simultaneously implement the desired high-level semantics for each independent high-level component.

In addition, a network is a distributed system, and all of the usual complications arise—in particular, control messages sent to switches are processed asynchronously. Programming asynchronous, distributed systems is notoriously difficult and error prone.  Network programmers require require support to get this right.

The goal of the Frenetic language is to raise the level of abstraction for programming SDNs. To replace the low-level imperative interfaces available today, Frenetic offers a suite of declarative abstractions for querying network state, deﬁning forwarding policies, and updating policies in a consistent way.  These constructs are designed to be *modular* so that individual policies can be written in isolation, by different developers and later composed with other components to create sophisticated policies. This is made possible in part by the design of the constructs themselves, and in part by the underlying run-time system, which implements them by compiling them down to low-level OpenFlow forwarding rules.  Our emphasis on modularity and composition—the foundational principles behind effective design of any complex software system—is the key feature that distinguishes Frenetic from other SDN controllers.

Preliminary NetCore Programming Concepts
-----------------------------------------

A NetCore policy describes how a collection of switches
forwards packets from one location to another.  We call a NetCore policy
*static* when it is fixed ahead of time,
does not change, and does not depend upon the packet flows that
appear in the network.  We will focus first on static policies 
and add some simple dynamics later in the tutorial.

The NetCore programming paradigm encourages users to think of static
policies as abstract *functions* that specify the behavior of switches
and to ignore how these functions are actually implemented on switch 
hardware --- our compiler will take
care of the implementation for you.
Conceptually, such static policies process *located packets* --- i.e.,
records with one field for each OpenFlow-supported packet header
(<code>srcMac</code>, <code>dstMac</code>, <code>srcIP</code>, etc.) as well as
one field denoting the current switch processing the packet and
another field denoting the inPort the packet arrived at.
More specifically, each policy is a function
that takes a single located packet as an input (the packet to
be forwarded) and generates a *multi-set* of new located packets.
(A multi-set is simply a set that can contain multiple, identical
elements.  When one takes the union of two multi-sets that both
contain the element x, the resulting multi-set has two occurrences
of the element x.)  In the rest of the tutorial, we will often 
call our located packets simply "packets" for short.
Keep in mind that all packets processed by NetCore come with associated
location information.

To understand how a packet flows through a network, a programmer must
analyze both the current NetCore policy P and the network topology
T.  The policy is a function that explains how a switch should move
a packet from an input port to an output port.  The topology is a
function that explains how a packet moves from the outport of one
switch, across a link, to the inport of some other switch.  Hence,
given a located packet p0, we can trace its path through the
network by first applying the policy function P(p0) generating a
multi-set of (possibly zero) packets {p1,...,pk} at outports on a
switch.  For simplicity, let's assume the result P(p0) contains
just one packet (p1) (i.e., it is a normal forwarding policy, that
does not drop the input packet and does not broadcast the packet out
multiple ports).  Next, we apply the topology function T to generate a
packet p1' across the other side of the link at some new switch.
Then we apply the policy function P again: P(p1') will generate some
subsequent number of output packets.  
And then apply the topology function T again.  

In summary, one traces the flow of
packets through a network by alternately applying the policy function
P and the topology function T.
Static NetCore is just a domain-specific
language that makes it easy to write down a single policy function
P that determines how switches forward packets.

Introductory Examples
---------------------

The main features of Static NetCore include the following.

  - a set of primitive *actions*, which allow programmers to modify and 
forward packets
  - *conditional statements*, which allow programmers to perform 
different actions on different kinds of packets
  - *sequencing*, which allows programmers to perform a series of
transformations on a packet, and
  - *parallel composition*, which allows programmers to make a logical copy
of a packet and thereby to generate more than one result from their
policy --- perhaps forwarding the packet to two different locations.

We will illustrate each of these features through a series of examples.

### Example 1: A Repeater

To begin, consider a network with just one switch.  Assume that switch 
has two ports, numbered 1 and 2.  Our first goal will be to program
a repeater --- a simple switch that forwards all packets coming in port 1
out port 2 and vice versa.  The following policy accomplishes that task.

```
(* a simple repeater *)

let repeater =
  if inPort = 1 then fwd(2)
  else fwd(1)
```
As in OCaml, 
NetCore comments are placed within <code>(*</code> and <code>*)</code>
(and comments may be nested).
The <code>let</code> keyword introduces a new policy, 
which we have chosen to call 
<code>repeater</code>.  
An <code>if</code>-<code>then</code>-<code>else</code> statement determines
whether to forward a packet out port 1 or port 2, depending on the packet's
<code>inPort</code> field.  In addition to testing the packet's <code>inPort</code>, 
if statement predicates can refer to
the <code>switch</code> 
at which a packet arrives, as well as any of the OpenFlow-supported
fields, such as the <code>srcIP</code>, <code>dstIP</code> or 
<code>frameType</code>.  
Conditions can also be formed using 
conjunctions (<code>&&</code>), disjunctions
(<code>||</code>) and negation (<code>!</code>) of other conditions.
See the [manual](link...) for the complete list of predicates. 

*Dave: I did this on my machine, not inside a final vm with all files
and aliases in place, etc, so this will need to be retested in the student 
environment.*

Now, let's test the program to see what it does.  In 
<code>$TUTORIALDIR/examples</code>,
you will see a file named <code>tut1.nc</code>, which contains the
repeater program.  Change to that directory and
start up the Frenetic controller program.

```
$ cd examples
$ frenetic tut1.nc
```

Now, in a separate shell, start up mininet:

```
$ sudo mn --controller=remote
*** Creating network
*** Adding controller
*** Adding hosts:
h1 h2 
*** Adding switches:
s1 
*** Adding links:
(h1, s1) (h2, s1) 
*** Configuring hosts
h1 h2 
*** Starting controller
*** Starting 1 switches
s1 
*** Starting CLI:
mininet> 
```

Mininet has started up a single switch with two hosts <code>h1</code> 
and <code>h2</code>, connected to the two ports on the switch.  At the 
mininet prompt, test your repeater program by pinging h2 from h1:

```
mininet> h1 ping -c 1 h2
```
You should see a trace like this one:
```
PING 10.0.0.2 (10.0.0.2) 56(84) bytes of data.
64 bytes from 10.0.0.2: icmp_req=1 ttl=64 time=0.216 ms

--- 10.0.0.2 ping statistics ---
1 packets transmitted, 1 received, 0% packet loss, time 0ms
rtt min/avg/max/mdev = 0.216/0.216/0.216/0.000 ms
```
Ping h1 from h2 as well.  Once you are convinced the repeater works,
try replacing the given repeater with an even simpler one:
```
let repeater = all
```
The <code>all</code> policy forwards any packet arriving at a switch out
all ports on that switch except the port it arrived on.  The "opposite"
of the <code>all</code> policy is the <code>drop</code> policy,
which drops all packets on the floor.  How would you create a policy that 
acts like a firewall, dropping certain packets and forwarding others?

### Example 2: Simple Port Translation

Next, let's adapt our first example so that instead of simply acting
as a repeater, our switch does some packet rewriting.  More specifically,
let's create a switch that maps connections initiated by host h1 
and destined to TCP port 5022 to the standard ssh port 22.  To implement
the example, we will use three additional concepts:  *modification
actions*, the *identity action* <code>pass</code> and *sequential composition*.

In general, packet modifications are written as follows:
```
field pre-value -> post-value
```
When executing such an action, the switch tests the <code>field</code>
to determine whether it holds the <code>pre-value</code>.  If it does,
then the field is rewritten to the <code>post-value</code>.  If it
does not, then the packet is dropped. *DPW: Check semantics of conditional
actions.*  For instance,
```
tcpDstPort 5022 -> 22
```
rewrites the <code>tcpDstPort</code> of packets starting with
<code>tcpDstPort</code> 5022 to 22.

Now that policies can have an interesting mix of modification and 
forwarding actions, we need a way to glue those actions together:
*sequential composition* (<code>;</code>).  For instance,
```
tcpDstPort 5022 -> 22; fwd(1)
```
modifies the <code>tcpDstPort</code> and then forwards the result of
the modification out port 1.  Note that in this case, we have composed
the effect of two actions.  However, you can use sequential composition
to compose the effects of any two policies --- they do not just have
to be simple actions.  This is useful for putting together complex
policies from simpler parts.  

The <code>pass</code> action goes together naturally with sequential
composition as it acts like the identity function on packets. In other
words, <code>pass</code> has the useful property that both 
<code>pass; P</code> and
<code>P; pass</code> are equal to <code>P</code> for any policy 
<code>P</code>.  At first, it seems as though this makes 
<code>pass</code> a completely useless construct, but it turns
out to be essential in combination with other features of NetCore.

Now, the port translation program:
```
let mapper =
  if inPort = 1 && tcpDstPort = 5022 then
    tcpDstPort 5022 -> 22
  else if inPort = 2 && tcpSrcPort = 22 then
    tcpSrcPort 22 -> 5022
  else
    pass

let forwarder =
  mapper; all
```
The mapper component rewrites the destination port in one
direction and the source port in the other, if those ports
take on the given values entering the switch.  If they don't,
<code>pass</code> leaves packets untouched.  A forwarder
is defined by composing a mapper with the all forwarding policy.

Try out the mapper from the examples directory:
```
$ frenetic tut2.nc
```
Then in mininet, in the default topology,
simulate an ssh process listening on port 22 on host h2:
```
mininet> h2 iperf -s -p 22 &
```
and connect to it from h1 by establishing a connection to port 5022.
The mapper will translate port numbers for you.
```
mininet> h1 iperf -c 10.0.0.2 -p 5022
```
You shoud see a trace like the following one.
```
------------------------------------------------------------
Client connecting to 10.0.0.2, TCP port 5022
TCP window size: 22.9 KByte (default)
------------------------------------------------------------
[  3] local 10.0.0.1 port 42605 connected with 10.0.0.2 port 5022
[ ID] Interval       Transfer     Bandwidth
[  3]  0.0-10.0 sec  3.24 GBytes  2.79 Gbits/sec
```

### Example 3: A Multi-Switch Network

Consider a network with 3 switches, and one host attached to each
switch: *TODO: draw a better picture*
```
    h1        h2        h3
    |         |         |
    |         |         |
    1         1         1
   (s1)2 -- 2(s2)3 -- 2(s3)
```
You can start such a network when you boot up mininet:
```
$ sudo mn --controller=remote --topo=linear,3
```
In this network, our goal is to flood arp packets sent over
the network and to route IP packets directly.  We'll write the
policy by breaking it in to pieces, starting with the components
that handle IP routing on a switch-by-switch basis.  In what
follows, we will use conditional <code>if pred then P</code> statements with 
no <code>else</code> branch.  Such statements apply <code>P</code> to all 
packets that satisfy <code>pred</code> and <code>drop</code> all others.
```
let s1 =
  if switch = 1 then 
    if dstIP = 10.0.0.1 then fwd(1)
    else fwd(2)

let s2 =
  if switch = 2 then 
    if dstIP = 10.0.0.1 then fwd(2)
    else if dstIP = 10.0.0.2 then fwd(1)
    else fwd(3)
   
let s3 =
  if (switch = 3) then 
    if dstIP = 10.0.0.1 || dstIP = 10.0.0.2 then fwd(2)
    else fwd(1)
```
Next, we define the main policy, which broadcasts arp traffic and
combine the three components defined above to forward IP traffic:
```
let router =
  if frameType = arp then all
  else s1 + s2 + s3
```
Here, <code>s1 + s2 + s3</code> represents the *parallel composition* of 
three policies. In other words, we execute all three policies simultaneously
on each packet without preference for one policy over another.  In this
specific case, if one policy (say s2) forwards the packet then the other
two will drop it (s1 and s3) because each policy handles a disjoint set
of packets (each handles the packets arriving at its respective switch).
This turns out to be what we want in this case, but it is perfectly
legal for multiple policies in a parallel composition to do something
interesting with a packet.  NetCore will ensure all actions are executed.
We will see this sort of composition in the next example.

To try out this policy, fire up mininet with the linear 3-switch topology:
```
$ sudo mn --controller=remote --topo=linear,3
```
Then start the <code>tut3.nc</code> program:
```
$ frenetic tut3.nc
```
In mininet, try starting a simple web server on host h3:
```
mininet> h3 python -m SimpleHTTPServer 80 &
```
And check that you can fetch content from the server at h3 from h1:
```
mininet> h1 wget -O - h3
```
Anything interesting on h3?

### Example 4: Queries and Debugging

Let's continue with example 3, but assume we were asleep when coding
and forgot to include the proper forwarding policy (s2) for switch 2.
Hence our router looks like this:
```
let router =
  if frameType = arp then all
  else s1 + s3  (* missing s2 *)
```
When we boot up the example, and try to ping h3 from h1, we are
unable to connect:
```
mininet> h1 ping -c 1 h3
PING 10.0.0.3 (10.0.0.3) 56(84) bytes of data.
From 10.0.0.1 icmp_seq=1 Destination Host Unreachable

--- 10.0.0.3 ping statistics ---
1 packets transmitted, 0 received, +1 errors, 100% packet loss, time 0ms
```
One good method to track down bugs in NetCore programs is
using wireshark.  However, another technique is to embed *queries*
in to NetCore programs themselves.  <code>monitorPackets( label )</code>
is a policy that sends all packets it receives to the controller,
as opposed to forwarding them along a data path.  The controller 
prints the packets to terminal prefixed by the 
string <code>label</code>.  

Typically, when one monitors a network, one does so *in parallel*
with some standard forwarding policy; one would like the packets
to go *two* places:  the controller, for inspection, and to whatever
else they are otherwise destined.  For instance, if we 
augment the broken variant of example 3 with several monitoring
clauses, composed in parallel with our router,
we can begin to diagnose the problem.  Notice also that we use
<code>if</code>-<code>then</code> statements to limit the packets
that reach the monitor policy --- the monitor policy
prints *all* packets that reach it.
```
let monitored_network = 
    router 
  + if switch = 2 && frameType = ip then monitorPackets("S2")
  + if switch = 3 && frameType = ip then monitorPackets("S3")
```
To see what happens, start up a controller running <code>tut4.nc</code>
```
$ frenetic tut4.nc
```
Now, inside mininet, ping host h3 from h1:
```
mininet> h1 ping -c 1 h3
```
In then controller terminal, you should see a packet 
arrive at switch 2, but no packets arrive at switch 3.
```
[s2] packet dlSrc=6a:a7:14:74:c8:95,dlDst=6a:aa:1c:52:e6:8f,nwSrc=10.0.0.1,nwDst=10.0.0.3,ICMP echo response on switch 2 port 2
```
Something must go wrong between the entry to switch 2 and the entry to switch 3!

Now, fix the policy by adding the s2 component back in to the 
<code>router</code> policy and try ping
again.  Note the differences.

If we are not interested in examining individual packets, but
instead are interested in monitoring the aggregate load at 
different places in the network,
we could use the <code>monitorLoad( n , label )</code> policy.  
This policy prints the number of packets and the number of 
bytes it receives every <code>n</code> seconds.  Again, each output
line is labelled with <code>label</code>, and
again, we can restrict the packets monitored by 
<code>monitorLoad</code> using a <code>if</code>-<code>then</code> clause.
For instance,
modifying <code>tut4.nc</code> to measure the load of non-arp traffic
on switch 3 every 5 seconds involves adding the following definition.
```
let monitored_network = 
  router + if switch=3 then monitorLoad(5, "LOAD")
```
To test the monitor, try using ping again.  Watch
the controller terminal to see how many packets cross switch 3.

### Exercises

In these exercises, we will experiment with a tree-shaped network of
3 switches and 4 hosts in the following configuration.
*TODO: better picture*
```
        (s1)
        1  2
       /    \
      3      3
  (s2)       (s3)
  1  2       1  2
 /    \     /    \
h1     h2  h3    h4
```
You can start up mininet in this configuration as follows.
```
$ sudo mn --controller=remote --mac --topo=tree,depth=2,fanout=2
```
Remember that the IP addresses of hosts h1, h2, h3, and h4 are
10.0.0.1, 10.0.0.2, 10.0.0.3 and 10.0.0.4, and the MAC addresses
are 1, 2, 3, and 4.

1.  Write the simplest possible policy that will allow any host to
send messages to any other.

2.  Set up a web server on host h4 and fetch a web page from h3.
How many arp packets (<code>frameType = arp</code>) reach switch s2?  How many 
IP packets (<code>frameType = ip</code>) reach switch s2?

3.  Refine your policy so that arp packets are broadcast to all hosts
but ip packets are forwarded only to their destination.

4.  Construct a network that forwards like the network defined in part 3
but also implements a firewall specified as follows.
  - Machines 30 and 40 are *trusted* machines.
  - Machines 10 and 20 are *untrusted* machines.
  - Set up a web server on each machine (TCP port 80).
  - Machines 30 and 40 have private files that may not be read by untrusted machines using HTTP.
  - Machines 10 and 20 have public files that may be read by any machine.
  - All traffic not explicitly prohibited must be allowed to pass through the network.

Solutions may be found in the guide/examples directory in 
files <code>sol1-1.nc</code>, <code>sol1-2.nc</code>, <code>sol1-3.nc</code>, 
<code>sol1-4.nc</code>.

*TODO:  Solutions! And polishing up the problems.  Would be nice
to think about how to implement some kind of "needle in a haystack" search
for some kind of traffic using queries.*

Dynamic NetCore Concepts
------------------------

So far in this tutorial, we have used NetCore to write static network
policies --- policies that do not change in response to network traffic or
topology events such as a switch coming up or going down.  In general,
crafting a dynamic policy amounts to writing a program that generates a
*stream* of static policies.  For instance, a new static policy can be generated
each time a new switch comes on line or the load in the network reaches
some threshold or a new connection is initiated.  

NetCoreDSL makes it possible to experiment with simple dynamic
policies by providing a small number of dynamic building blocks
including a learning switch and a NAT box.  Intuitively, to create
a dynamic policy, one writes an ordinary static policy that 
includes a reference to a dynamic building block:

```
let policy = ... static_component ... dynamic_component ...
```
If the <code>dynamic_component</code> generates the following series of 
policies as it executes:
```
dynamic1

dynamic2

dynamic3 

...
```
then <code>policy</code> will be:
```
... static_component ... dynamic1 ...

... static_component ... dynamic2 ...

... static_component ... dynamic3 ...

...
```
In other words, the static components remain fixed as the dynamic subcomponent
fluctuates from one variant to the next.


### Example 5: NAT

In this example, we will investigate how to use a dynamic NAT
component to support connections initiated from a private machine "on
the inside," behind a public IP, to a remote machine "on the outside."
More specifically, when a machine on the inside initiates a connection
to a machine on the outside, NAT will pick a new, available public
port, and rewrite the source IP and port to use the available port and
the public IP. Responses to previously established port are rewritten
to use the private source port and IP.

To use the built-in <code>nat</code> box, we supply it with a desired public IP
address (say, <code>10.0.0.254</code>) and invoke it as follows.
```
let translatePrivate, translatePublic = nat (publicIP = 10.0.0.254)
in ...
```
This expression generates a pair of dynamic 
components:  (1) <code>translatePrivate</code>
rewrites requests travelling from inside to 
outside, and (2) <code>translatePublic</code> rewrites requests travelling
from outside back in.  These two components can be used within the
policy following the keyword <code>in</code>.  The following
code defines the complete program. 
```
let natter =
  let translatePrivate, translatePublic = 
    nat (publicIP = 10.0.0.254) 
  in
    if switch = 1 && inPort = 1 then 
      (translatePrivate; if inPort = 1 then fwd(2) else pass)
  + if switch = 1 && inPort = 2 then
      (translatePublic; if inPort = 2 then fwd(1) else pass)

let app =
  if frameType = arp then all
  else monitorTable(1, natter)  
```
The last line of the program uses <code>monitorTable(n,pol)</code>,
which will print the flow table generated for switch number
<code>n</code> due to <code>pol</code> each time the policy
is updated.  This gives the programmer insight in to which rules 
are installed dynamically on the switch.

The policy can be found in <code>tut5.nc</code>.  Launch a controller
now:
```
$ frenetic tut5.nc
```
and start mininet as follows.
```
$ sudo mn  --controller=remote --mac
```
Next you will need to prime your arp cache on host <code>h2</code>
and start a simple web server running.
```
mininet> h2 arp -s 10.0.0.254 00:00:00:00:00:01
mininet> h2 python -m SimpleHTTPServer 80 . &
```
Finally, try fetching a web page from the server:
```
h1 wget -O - 10.0.0.2
```
When you do so, you should see the <code>tut5.nc</code> controller
print out an updated flow table for switch 1.  The flow table is 
printed as a list of rules, with the highest priority rule at the
top and the lowest priority rule at the bottom.  Each rule has
the form
```
{Packet Pattern} => [Actions]
```
The <code>Packet Pattern</code> is a list of conditions the packet
fields must satisfy to match the rule.  The <code>Actions</code>
are taken if the packet matches.  

After issuing the <code>wget</code> command,
you should see 2 rules matching packets coming <code>inPort</code> 1
and 2 rules matching packets coming <code>inPort</code> 2.
Take a look:  Which private port did this connection use?  Which
public port?

### Exercises

*This section very much TODO.*

1. When running tut5.nc, what happens if you try to ping h2 from h1?
```
mininet> h1 ping -c 1 h2
```
Modify the policy in some way to allow ping through.

2. Experiment with the Mac Learning Component.  A simple policy that
uses Mac Learning may be found in tut6.nc.
Consider running the policy on our tree-shaped topology.
*TODO: better picture*
```
        (s1)
        1  2
       /    \
      3      3
  (s2)       (s3)
  1  2       1  2
 /    \     /    \
h1     h2  h3    h4
```
This time start up mininet with random mac addresses (omit the
<code>--mac</cod> option.  
```
$ sudo mn --controller=remote --topo=tree,depth=2,fanout=2
```
Figure out what the mac addresses of each of the hosts are using
two different monitoring techniques.

Solutions to these exercises may be found in
guide/examples directory in files <code>sol2-1.nc</code> and
<code>sol2-2.nc</code>.

Summary
-------

NetCore rocks!  QED.
