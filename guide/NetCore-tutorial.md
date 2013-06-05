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
built using [NetCoreLib](https://github.com/frenetic-lang/frenetic/tree/master/src/NetCoreLib),
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
There are also a number of exercises for the reader.  As you read this
document, we encourage you to experiment with the examples and try the
exercises.  To get started, you will need to download and start up the
tutorial VM.  Please see the instructions here.

*TO DO: Link to joint getting started instructions*

Motivation for the Frenetic and NetCore Designs
-----------------------------------------------

*Much in this section was plagiarized from IEEE overview paper*

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
> cd examples
> frenetic tut1.nc
```

Now, in a separate shell, start up mininet:

```
> sudo mn --controller=remote
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

Mininet has started up a single switch with two hosts <code>h1</code> and
<code>h2</code>, connected to the two ports on the switch.
At the mininet prompt, test your repeater program by pinging h2
from h1:

```
mininet> h1 ping h2
```

You should see a trace like this one:

```
64 bytes from 10.0.0.2: icmp_req=1 ttl=64 time=0.397 ms
64 bytes from 10.0.0.2: icmp_req=2 ttl=64 time=0.051 ms
64 bytes from 10.0.0.2: icmp_req=3 ttl=64 time=0.059 ms
64 bytes from 10.0.0.2: icmp_req=4 ttl=64 time=0.056 ms
64 bytes from 10.0.0.2: icmp_req=5 ttl=64 time=0.040 ms
64 bytes from 10.0.0.2: icmp_req=6 ttl=64 time=0.042 ms
^C
--- 10.0.0.2 ping statistics ---
6 packets transmitted, 6 received, 0% packet loss, time 5000ms
rtt min/avg/max/mdev = 0.040/0.107/0.397/0.130 ms
```

Ping h1 from h2 as well.  Once you are convinced the repeater works,
try replacing the given repeater with an even simpler one:

```
let repeater = all
```

The <code>all</code> policy forwards any packet arriving at a switch out
all ports on that switch except the port it arrived on.


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
> frenetic tut2.nc
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

### Example 3: A Slightly Larger Network

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
> sudo mn --controller=remote --topo=linear,3
```
In this network, our goal is to flood arp packets sent over
the network and to route IP packets directly.  We'll write the
policy by breaking it in to pieces, starting with the components
that handle IP routing on a switch-by-switch basis:
```
let s1 =
  filter (switch = 1);
  if dstIP = 10.0.0.1 then fwd(1)
  else fwd(2)

let s2 =
  filter (switch = 2);
  if dstIP = 10.0.0.1 then fwd(2)
  else if dstIP = 10.0.0.2 then fwd(1)
  else fwd(3)
    
let s3 =
  filter (switch = 3);
  if dstIP = 10.0.0.1 || dstIP = 10.0.0.2 then fwd(2)
  else fwd(1)
```
In the program fragment above, we have used a new feature: the filter.  A filter
selects those packets that match the associated predicate (in this case
a predicate on switch id) and allows them to pass through.  It drops
all packets that do not match match the filter.

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
In the next example, we will see parallel composition used to do multiple
interesting things with the same packet.

To try out this policy, fire up mininet with the linear 3-switch topology:
```
> sudo mn --controller=remote --topo=linear,3
```
Then start the <code>tut3.nc</code> program:
```
> frenetic tut3.nc
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
mininet> h1 ping h3
^CPING 10.0.0.3 (10.0.0.3) 56(84) bytes of data.

--- 10.0.0.3 ping statistics ---
12 packets transmitted, 0 received, 100% packet loss, time 11029ms
```
One good method to track down bugs in NetCore programs is
using wireshark.  However, another technique is to embed *queries*
in to NetCore programs themselves.  <code>monitorPackets( pred )</code>
is a policy that siphons off packets to the controller and prints them
to the terminal, dropping all other packets.  For instance, 
If we augment the broken variant of example 3 with a monitor,
we can begin to diagnose the problem:
```
let monitored_network = 
  router + monitorPackets(switch=2)
```
We use parallel composition here as we wish to make two copies of a packet:
one copy is processed forwarded by the router and another copy is processed
by the monitor.

To see what happens, start up a controller running <code>tut4.nc</code>
```
> frenetic tut4.nc
```
Now, inside mininet, ping host h3 from h1:
```
mininet> h1 ping h3
^CPING 10.0.0.3 (10.0.0.3) 56(84) bytes of data.

--- 10.0.0.3 ping statistics ---
7 packets transmitted, 0 received, 100% packet loss, time 6039ms
```
In your controller terminal, you should see a stream of packets being
received at switch 2.
```
...
Packet (ae:3d:a5:8a:d5:e1, 1e:b0:be:9b:99:2b, 2048, none, 0, Not yet implemented) on switch 2 port 2 matched filter switch = 2
Packet (ae:3d:a5:8a:d5:e1, 1e:b0:be:9b:99:2b, 2048, none, 0, Not yet implemented) on switch 2 port 2 matched filter switch = 2
Packet (ae:3d:a5:8a:d5:e1, 1e:b0:be:9b:99:2b, 2048, none, 0, Not yet implemented) on switch 2 port 2 matched filter switch = 2
```
So the packets are arriving at switch 2.  If you adjust your monitor to
monitor switch 3, you'll see the packets don't make it.  
Something goes wrong between the entry to switch 2 and the entry to switch 3!
Now, fix the policy:
```
let router =
  if frameType = arp then all
  else s1 + s2 + s3
```
and let's monitor the load on the network instead of looking at the contents
of packets.  Doing so involves <code>monitorLoad( n , pred )</code>,
which prints the number of packets and the number of 
bytes satisfying 
<code>pred</code> every <code>n</code> seconds.  
Modifying <code>tut4.nc</code> to measure the load of non-arp traffic
on switch 3 every 5 seconds involves adding the following definition.
```
let monitored_network = 
  router + monitorLoad(5, switch=3)
```
To test the monitor, try pinging from h1 to h2 as well as h1 to h3.  Watch
the controller terminal to see how many packets cross switch 3 in 
each case.

Exercises
---------

Semantics
---------

Summary
---------

Dynamic NetCore Concepts
------------------------

- A dynamic program produces a stream of policies.

Dynamic NetCore Programing 1: NAT
----------------------------------

We have to explain how NAT works.
- When a machine on the inside initiates a connection to a machine on the outside, NAT will pick a new, available public port, and rewrite the source IP and port to use the available port and the public IP. Responses to previously established port are rewritten to use the private source port and IP.


1. Keep routing component from (3) above.

2. Place a NAT on switch 101, with port 1 as public-port and port 2 as private-port. 

3. Initiate a Web connection from host 10 to the untrusted host 50.

```mininet
mininet> h50 cat /usr/share/dict/words | nc -l 80 &
mininet> h10 curl 10.0.0.50 # should print words
```
 
4. Determine the public and private ports that this connection uses, using monitor_tbl / monitor_pol

5. Run a Web server on h30:

```mininet
mininet> h30 cat /usr/share/dict/words | nc -l 80 &
```

6. Connect to the Web server from h10 to ensure that it is working:

```mininet
mininet> h10 curl 10.0.0.30 # should print words
```

7. Can you connect from h50? Why not?

```mininet
mininet> h20 curl 10.0.0.30 # should hang (hit Ctrl + C)
```

8. Compose the current policy with a new policy that allows connections from outside the NAT to h30.


Dynamic NetCore Programing 2: Mac-Learning
------------------------------------------

Explain how Mac-Learning works (composition)

1. Keep the NAT from the previous section, but modify the routing policy as follows:
  - Use a static policy to route between 101 and 102.
  - Use mac-learning for all other routing.
2. Inspect the mac-learning table on switch 102.
3. ...


Compilation Instructions
=========================

To execute this document as a NetCore program, do the following:

The following line determines the policy to be executed.  Replace **learn**
with the name of some other policy defined in this file to test it out.

```
policy main = learn
```
