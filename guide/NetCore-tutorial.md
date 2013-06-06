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

As an aside, you may find it interesting to read about the [motivation and
design decisions](http://frenetic-lang.org/publications/overview-ieeecoms13.pdf)
that went into creating NetCore and Frenetic.

Getting Started
---------------

As you read this document, we encourage you to experiment with the examples 
and try the exercises.  To do so, you will need to download and start up the
tutorial VM.  Please see the [instructions here](https://github.com/frenetic-lang/frenetic/blob/master/guide/01-Introduction.md#getting-started).

Chapter 1: Introducing NetCore
------------------------------

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
care of the implementation for you.  Conceptually, such static 
policies process *located packets* --- i.e.,
records with one field for each OpenFlow-supported packet header
(<code>srcMac</code>, <code>dstMac</code>, <code>srcIP</code>, etc.) as well as
one field denoting the current switch processing the packet and
another field denoting the inPort the packet arrived at.
More specifically, each policy is a function
that takes a single located packet as an input (the packet to
be forwarded) and generates a *multi-set* of new located 
packets.  (A multi-set is simply a set that can contain multiple, identical
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
P and the topology function T. Static NetCore is just a domain-specific
language that makes it easy to write down a single policy function
P that determines how switches forward packets. The main features of 
Static NetCore include the following.

  - a set of primitive *actions*, which allow programmers to modify and 
forward packets,
  - *conditional statements*, which allow programmers to perform 
different actions on different kinds of packets,
  - *sequencing*, which allows programmers to perform a series of
transformations on a packet,
  - *parallel composition*, which allows programmers to make a logical copy
of a packet and thereby to generate more than one result from their
policy --- perhaps forwarding the packet to two different locations, and
  - *queries*, which allow programmers to inspect

We will illustrate each of these features through a series of examples.
You will find the examples in the frenetic repository 
in <code>guide/netcore-tutorial-code</code>.  Go there now.
```
$ cd guide/netcore-tutorial-code
```

### Example 1: A Naive Repeater (Redux)

In the [OxRepeater](02-OxRepeater.md) chapter, you learned how to program an
efficient repeater by adding rules to the switch flow table.  Recall that a
repeater simply forwards incoming packets on all other ports.

In this example, we will begin by considering a network with just one switch
with two ports, numbered 1 and 2.  Our first goal will be to program a
switch-specific repeater that forwards traffic arriving at port 1 out port 2,
and vice versa.  The following NetCore policy accomplishes that task.

```
(* a simple repeater *)

let repeater =
  if inPort = 1 then fwd(2)
  else fwd(1) in
monitorTable(1, repeater)
```

As in OCaml, NetCore comments are placed within <code>(*</code> and
<code>*)</code> (and comments may be nested). The <code>let</code> keyword
introduces a new policy, which we have chosen to call <code>repeater</code>.
An <code>if</code>-<code>then</code>-<code>else</code> statement determines
whether to forward a packet out port 1 or port 2, depending on the packet's
<code>inPort</code> field.  In addition to testing the packet's
<code>inPort</code>, if statement predicates can refer to the
<code>switch</code> at which a packet arrives, as well as any of the
OpenFlow-supported fields, such as the <code>srcIP</code>, <code>dstIP</code>
or <code>frameType</code>.  Conditions can also be formed using conjunctions
(<code>&&</code>), disjunctions (<code>||</code>) and negation (<code>!</code>)
of other conditions.  See the [manual](link...) for the complete list of
predicates. 

The last line of the program uses <code>monitorTable(1,repeater)</code>, which
will print the flow table generated for switch <code>1</code> from the
<code>repeater</code> policy.  It is equivalent to <code>repeater</code>, but
with the side effect of printing the flow table.  Now, when you run the
example, take a look at the flow table that the NetCore compiler creates for
you and compare it to your flow table rules from the Ox tutorial.

#### Run the Example

Within the <code>netcore-tutorial-code</code> directory, you should
find the repeater policy in <code>repeater.nc</code>.  To start the
repeater controller, just type:
```
$ frenetic repeater.nc
```
Now, in a separate terminal, start up mininet with the default, single
switch topology.
```
$ sudo mn --controller=remote
```
You should see the following trace as mininet boots up.
```
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
and <code>h2</code>, connected to the two ports on the switch.  
If you ever want to know more about the topology, you can type
```
mininet> net
```
You should see the following.
```
c0
s1 lo:  s1-eth1:h1-eth0 s1-eth2:h2-eth0
h1 h1-eth0:s1-eth1
h2 h2-eth0:s1-eth2
```
Line 1 tells you there is a controller (<code>c0</code>) running.  Line 2
describes the ports on switch <code>s1</code>.  In particular, 
switch 1 port 1 (<code>s1-eth1</code>) is connected to host h1.
Likewise, switch 1 port 2 (<code>s1-eth2</code>) is connected to 
host h2. If there was more than one switch in the network, we would
see additional lines prefixed by the switch identifier, one line
per switch.  Lines 3 and 4 describe the hosts <code>h1</code> 
and <code>h2</code>.

#### Test the Example

At the mininet prompt, test your repeater program by pinging h2 from h1:
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
Ping h1 from h2 as well.
Once you are convinced the repeater works,
try replacing the given repeater with an even simpler one:
```
let repeater = all in
monitorTable(1, repeater)
```
The <code>all</code> policy forwards any packet arriving at a switch out
all ports on that switch except the port it arrived on.  Try testing
that out too to see if you have done it correctly.

The opposite of the <code>all</code> policy is the <code>drop</code> policy,
which drops all packets on the floor.  

### Exercise 1: Firewall

In the [OxFirewall](03-OxFirewall.md) chapter, you developed a firewall to
block ICMP traffic.  Most networks impose other restrictions on the type of
traffic that hosts are allowed to send.  The following table describes the type
of traffic that each host in a five-host network can send:

<table>
  <TR> 
    <TH>Host</TH> <TH>Host Description</TH> 
    <TH>frameType</TH> <TH>ipProtocolType</TH> <TH>tcpDstPort</TH> 
  </TR>
  <TR> 
    <TD>H1</TD> <TD>Network tap: receives traffic.</TD>
    <TD>arp</TD> <TD></TD> <TD></TD>           
  </TR>
  <TR> 
    <TD>H2</TD> <TD>Admin.</TD>
    <TD>*</TD> <TD>*</TD> <TD>*</TD>           
  </TR>
  <TR> 
    <TD>H3</TD> <TD>User: web traffic.</TD>
    <TD>arp, ip</TD> <TD>ipv4</TD> <TD>80</TD>          
  </TR>
  <TR> 
    <TD>H4</TD> <TD>User: web traffic.</TD>
    <TD>arp, ip</TD> <TD>ipv4</TD> <TD>80</TD>          
  </TR>
  <TR> 
    <TD>H5</TD> <TD>Power user: web traffic, ssh, and ping.</TD>
    <TD>arp, ip</TD> <TD>icmp, ipv4</TD> <TD>22, 80</TD>           
  </TR>
</table>

For example, H4 is allowed to send ARP and web traffic, whereas H5 can ping, as
well as send ARP, web, and SSH traffic.  H1 is a network tap: it can send ARP
traffic to advertise its location, but otherwise receives and logs diagnostic
traffic directed to it.  The administrator can, of course, send any type of
traffic.

#### Programming Task

Write a NetCore policy for a network with a single switch and five hosts
connected to ports 1-5, respectively, that enforces the restrictions in the
table above.  Assume that any traffic that meeting the criteria may be
broadcast (i.e. using the <code>all</code> policy).

*TODO: make this link work.*

Use [Firewall.nc](netcore-tutorial-code/Firewall.nc) as a starting point.

#### Testing your Controller

To run your controller, navigate to the <code>netcore-tutorial-code</code>
directory and type:
```
frenetic Firewall.nc
```

In another terminal, start a mininet instance with five hosts:
```
sudo mn --controller=remote --topo=single,5
```

##### ICMP

Use <code>ping</code> to test ICMP.  Remember that ICMP traffic is
bidirectional, and so pinging from, say, H2 to H3 should fail, because H3
cannot reply.

##### IPv4

Use <code>iperf</code> to test both SSH and web traffic:
```
mininet> h2 iperf -s -p 80 &
mininet> h3 iperf -c 10.0.0.3 -p 80
```

The first command sets <code>iperf</code> to listen on port 80 on host H3.  The
second initiates a TCP connection from H4 to 10.0.0.3:80 (H3, port 80).  You
should see the following output:
```
mininet> h2 iperf -s -p 80 &
------------------------------------------------------------
Server listening on TCP port 80
TCP window size: 85.3 KByte (default)
------------------------------------------------------------
mininet> h3 iperf -c 10.0.0.3 -p 80
------------------------------------------------------------
Client connecting to 10.0.0.3, TCP port 80
TCP window size:  647 KByte (default)
------------------------------------------------------------
[  3] local 10.0.0.3 port 60621 connected with 10.0.0.3 port 80
[  4] local 10.0.0.3 port 80 connected with 10.0.0.3 port 60621
[ ID] Interval       Transfer     Bandwidth
[  3]  0.0-10.0 sec   384 MBytes   322 Mbits/sec
[ ID] Interval       Transfer     Bandwidth
[  4]  0.0-10.0 sec   384 MBytes   322 Mbits/sec
```

##### ARP

Testing H1 is a bit tricky, as it should respond to ARP requests but not, say,
TCP handshakes.  One approach is to use <code>iperf</code> to receive UDP
traffic on H1:
```
mininet> h1 iperf -u -s -p 80 &
mininet> h2 iperf -u -c 10.0.0.1 -p 80
```

Sending UDP traffic from H2 to H1 should succeed with the following output:
```
mininet> h1 iperf -u -s -p 80 &
------------------------------------------------------------
Server listening on UDP port 80
Receiving 1470 byte datagrams
UDP buffer size:  176 KByte (default)
------------------------------------------------------------
mininet> h2 iperf -u -c 10.0.0.1 -p 80
------------------------------------------------------------
Client connecting to 10.0.0.1, UDP port 80
Sending 1470 byte datagrams
UDP buffer size:  176 KByte (default)
------------------------------------------------------------
[  3] local 10.0.0.2 port 40281 connected with 10.0.0.1 port 80
[ ID] Interval       Transfer     Bandwidth
[  3]  0.0-10.0 sec  1.25 MBytes  1.05 Mbits/sec
[  3] Sent 893 datagrams
[  3] WARNING: did not receive ack of last datagram after 10 tries.
```

The warning on the last line indicates that the acknowledgment from H1 did not
reach H2, as expected.


Chapter 2: Composition Operators
=================================

A key feature of the NetCore design is its support for modular
construction of SDN policies:  One can build complex policies
by combining a collection of simpler ones.  In this chapter,
we will describe two key *composition operators* that make this
possible.

A Port Mapping Policy
----------------------

To begin, let's adapt the example from Chapter 1 
so that instead of simply acting
as a repeater, our switch does some packet rewriting.  More specifically,
let's create a switch that maps connections initiated by host h1 
and destined to TCP port 5022 to the standard ssh port 22.  

In general, packet modifications are written as follows:
```
field pre-value -> post-value
```
When executing such an action, the switch tests the <code>field</code>
to determine whether it holds the <code>pre-value</code>.  If it does,
then the field is rewritten to the <code>post-value</code>.  If it
does not, then the packet is dropped.  For instance,
```
tcpDstPort 5022 -> 22
```
rewrites the <code>tcpDstPort</code> of packets starting with
<code>tcpDstPort</code> 5022 to 22.

Now that policies can have an interesting mix of modification and 
forwarding actions, we need a way to take the outputs of one policy
and funnel them in to the inputs of another policy.  This is exactly
what the *sequential composition operator* (<code>;</code>)
does.  For instance,
```
tcpDstPort 5022 -> 22; fwd(1)
```
modifies the <code>tcpDstPort</code> and then forwards the result of
the modification out port 1.  Note that in this case, we have composed
the effect of two actions.  However, you can use sequential composition
to compose the effects of any two policies --- they do not just have
to be simple actions.    

We need one more concept in order to be able to write our port-mapper
program elegantly:  The <code>pass</code> action.  This action
acts like the identity function on packets. In other
words, it simply pipes all of its input packets through untouched
to its output.  Hence, <code>pass</code> has the property that both 
<code>pass; P</code> and
<code>P; pass</code> are exactly the same as just <code>P</code>, for any 
policy <code>P</code>.  At first, it seems as though this makes 
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

let routing = 
  all

let forwarder =
  mapper; routing
```
The mapper component rewrites the destination port in one
direction and the source port in the other, if those ports
take on the given values entering the switch.  Notice how we
used <code>pass</code> in the final else branch of the 
<code>mapper</code> policy to leaves packets of all other kinds
untouched.  Doing so, allows us to compose the mapper component
with any routing component we choose.  In this case, a forwarder
is defined by composing a mapper with the trivial <code>all</code>
routing policy.  However, in a more complex network, the routing
component could be arbitrarily sophisticated and still compose
properly with the mapper.

Testing the Port Mapper
-----------------------

You will find the mapper in <code>port_map.nc</code>.  Start it up
with frenetic.
```
$ frenetic port_map.nc
```
Then start mininet in the default topology, and
simulate an ssh process listening on port 22 on host h2:
```
$ sudo mn --controller=remote
mininet> h2 iperf -s -p 22 &
```
You can connect to h2 (IP address 10.0.0.2) 
from h1 by establishing a connection to port 5022
using the command below.  (The <code>-t</code> option specifies
the time window for sending traffic.)
The mapper will translate port numbers for you.
```
mininet> h1 iperf -c 10.0.0.2 -p 5022 -t 0.0001
```
You shoud see a trace like the following one.
```
------------------------------------------------------------
Client connecting to 10.0.0.2, TCP port 5022
TCP window size: 22.9 KByte (default)
------------------------------------------------------------
[  3] local 10.0.0.1 port 52273 connected with 10.0.0.2 port 5022
[ ID] Interval       Transfer     Bandwidth
[  3]  0.0- 0.0 sec   128 KBytes   149 Mbits/sec
```
You can also test that the network still allows ping traffic
through:
```
mininet> h1 ping -c 1 h2
```

Composing Queries
-----------------

When developing more complex policies, it is very useful to be able
peer in to the middle of the network to take a look at what is going
on.  Hence, NetCore supports several kinds of queries that can help you
understand and debug the behavior of your network.  As an example,
the <code>monitorPackets( label )</code> policy sends every input packet
it receives to the controller as opposed to forwarding it along a
network data path (like the <code>fwd(port)</code> policy does).
At the controller, the packet is printed with the string <code>label</code> 
as a prefix and then discarded.

Interestingly, when one monitors a network, one does so *in parallel*
with some standard forwarding policy; one would like the packets
to go *two* places:  the controller, for inspection, and to whatever
else they are otherwise destined.  To support this kind of idiom,
we must introduce a new kind of operator on policies:  
*parallel composition*.  Intuitively, when supplied with a packet
<code>p</code> as input, the parallel composition 
<code>P1 + P2</code> applies <code>P1</code> to <code>p</code>
and also applies <code>P2</code> to <code>p</code>.  
Overall, it generates 
the *union* of the results from <code>P1</code> and <code>P2</code>.  Hence, 
if <code>P1</code> forwards to A
and <code>P1</code> forwards to B then <code>P1 + P2</code> makes
a copy of the input packet and forwards to both A and B.

With this in mind, let's modify the port mapper to inspect the
packets both before and after the rewriting.  To do so, we can
leave the mapper component unchanged from above but add 
monitoring to the forwarder.
```
...
let before = if inPort = 1 then monitorPackets("BEFORE")

let after = if inPort = 1 then monitorPackets("AFTER")

let forwarder =
  (before + mapper); (all + after)
```
Above, we used a 
<code>if</code>-<code>then</code> statement (no else) to limit the packets
that reach the monitoring policy to only those packets satisfying
the <code>inPort = 1</code> predicate.  Otherwise, the monitor policy
prints *all* packets that reach it.  Note that if there is no <code>else</code>
branch in a conditional, packets not matching the conditional are dropped.

This new policy can be found in <code>port_map_monitor.nc</code>.
Test it as above using iperf, but this time watch the output in the 
controller window.  You should see lines similar to the following being printed:
```
[BEFORE] packet dlSrc=4a:f7:98:81:78:0d,dlDst=d6:7c:1e:d6:e3:0b,nwSrc=10.0.0.1,nwDst=10.0.0.2,tpSrc=52923;tpDst=5022 on switch 1 port 1
[AFTER] packet dlSrc=4a:f7:98:81:78:0d,dlDst=d6:7c:1e:d6:e3:0b,nwSrc=10.0.0.1,nwDst=10.0.0.2,tpSrc=52923;tpDst=22 on switch 1 port 1
```
You will notice <code>tpDst=5022</code> in lines marked
<code>BEFORE</code> and <code>tpDst=22</code> in lines marked 
<code>AFTER</code>.

Another useful kind of query is one that measures the load at different
places in the network.  The <code>monitorLoad( n , label )</code> policy  
prints the number of packets and the number of 
bytes it receives every <code>n</code> seconds.  Again, each output
line is prefixed by the string <code>label</code>, and
again, we can restrict the packets monitored by 
<code>monitorLoad</code> using a <code>if</code>-<code>then</code> clauses.
Try removing the packet monitoring policies and in the port map 
application and adding a <code>monitorLoad</code> policy
to measure the number of packets
sent by iperf.  The implementation of <code>monitorLoad</code>
is far more efficient than <code>monitorPackets</code>
as it does not send packets to the controller.  It merely queries
openFlow counters after each time interval.  You can issue a longer
iperf request by adjusting the timing parameter.  Watch the load
printed in the controller window.  The following command runs
iperf for <code>20</code> seconds.
```
mininet> h1 iperf -c 10.0.0.2 -p 5022 -t 20
```

Programming Exercise: A Multi-Switch Network
---------------------------------------------

In this exercise, you will explore how to construct a policy for
a multi-switch network using the composition operators discussed
in this chapter.  You should also practice using queries to help
debug your program.  The network under consideration has 3 switches, 
and one host attached to each switch: *TODO: draw a better picture*
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
Your goals are to implement a policy that performs the following
actions:
  - broadcast all arp packets to all hosts
  - route other packets to the correct host according to their destination 
IP address.  Host h1 has IP address 10.0.0.1, host h2 has
IP address 10.0.0.2 and host h3 has IP address 10.0.0.3.
  - prevent host h2 from contacting h3's web server.  In other words,
drop all packets from source IP 10.0.0.2 destined for source
IP 10.0.0.3 on tcp port 80 (web).

Moreover, the goal is to design the policy in a modular fashion.
To that end, we have provided you with a template to start from in
<code>Multi_Switch.nc</code>.  The template defines independent components
that should implement IP routing for switches s1, s2, and s3.  There is
also a component for defining a firewall policy.  After defining the
individual components, assemble them using the appropriate
composition operators.

To test your policy, fire up mininet with the linear 3-switch topology:
```
$ sudo mn --controller=remote --topo=linear,3
```
Then start your <code>Multi_Switch.nc</code> program:
```
$ frenetic tut3.nc
```
In mininet, start a web server on host h3:
```
mininet> h3 python -m SimpleHTTPServer 80 . &
```
Check that you can fetch content from the server at h3 from h1:
```
mininet> h1 wget -O - h3
```
But not from h2:
```
mininet> h2 wget -O - h3
```
Ensure that you are correctly forwarding other traffic 
by using ping or iperf.

Chapter 3:  Dynamic NetCore
===========================

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
