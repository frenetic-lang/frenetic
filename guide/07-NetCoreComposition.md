Chapter 7: Composition Operators
=================================

A key feature of the NetCore design is its support for modular
construction of SDN policies:  One can build complex policies
by combining a collection of simpler ones.  In this chapter,
we will describe two key *composition operators* that make this
possible.

A Port Mapping Policy
----------------------

To begin, let's adapt the example from Chapter 6 so that instead of simply
acting as a repeater, our switch does some packet rewriting.  More
specifically, let's create a switch that maps connections initiated by host H1
and destined to TCP port 5022 to the standard SSH port 22.  

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
rewrites the <code>tcpDstPort</code> of packets from 5022 to 22.

Now, policies can mix modification and forwarding actions, but we need a way to
pipe the output of one policy into the next.  For this, we use
the *sequential composition* operator (<code>;</code>).  For instance,
```
tcpDstPort 5022 -> 22; fwd(1)
```
rewrites the <code>tcpDstPort</code> and then forwards the modified packets out
port 1.  In this case, we have composed the effects of two actions, but in
general you can use sequential composition to compose any two policies.

We need one more concept in order to write an elegant port-mapper program:
The <code>pass</code> action.  This action acts like the identity function on
packets. In other words, it simply pipes all of its input packets through
untouched to its output.  Hence, <code>pass</code> has the property that both
<code>pass; P</code> and <code>P; pass</code> are exactly the same as just
<code>P</code>, for any policy <code>P</code>.  At first, it seems as though
this makes <code>pass</code> a completely useless construct, but it turns out
to be essential in combination with other features of NetCore.

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
The <code>mapper</code> component rewrites the destination port in one
direction and the source port in the other, if those ports
take on the given values entering the switch.  Notice how we
used <code>pass</code> in the final <code>else</code> branch of the 
<code>mapper</code> policy to leaves packets of all other kinds
untouched.  This allows us to compose the mapper component
with any routing component we choose.  In this case, <code>forwarder</code>
is defined by composing <code>mapper</code> with the trivial <code>all</code>
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
simulate an SSH process listening on port 22 on host H2:
```
$ sudo mn --controller=remote
mininet> h2 iperf -s -p 22 &
```
You can connect to H2 (IP address 10.0.0.2) 
from H1 by establishing a connection to port 5022
using the command below.  (The <code>-t</code> option specifies
the time window for sending traffic.)
The mapper will translate port numbers for you.
```
mininet> h1 iperf -c 10.0.0.2 -p 5022 -t 0.0001
```
You shoud see a trace like the following:
```
------------------------------------------------------------
Client connecting to 10.0.0.2, TCP port 5022
TCP window size: 22.9 KByte (default)
------------------------------------------------------------
[  3] local 10.0.0.1 port 52273 connected with 10.0.0.2 port 5022
[ ID] Interval       Transfer     Bandwidth
[  3]  0.0- 0.0 sec   128 KBytes   149 Mbits/sec
```
You can also test that the network still allows ping traffic:
```
mininet> h1 ping -c 1 h2
```

Composing Queries
-----------------

When developing more complex policies, it is very useful to peer into the
middle of the network.  Hence, NetCore supports several kinds of queries that
can help you understand and debug the behavior of your network.  As an example,
the <code>monitorPackets( label )</code> policy sends every input packet it
receives to the controller as opposed to forwarding it along a network data
path (like the <code>fwd(port)</code> policy does).  At the controller, the
packet is printed with the string <code>label</code> as a prefix and then
discarded.

Interestingly, when one monitors a network, one does so *in parallel* with some
standard forwarding policy; one would like the packets to go *two* places:  the
controller, for inspection, and wherever they may be destined in the network.
To support this idiom, we must introduce a new kind of operator on policies:
*parallel composition*.  Intuitively, when supplied with a packet
<code>p</code> as input, the parallel composition <code>P1 + P2</code> applies
<code>P1</code> to <code>p</code> and also applies <code>P2</code> to
<code>p</code>.  Overall, it generates the *union* of the results from
<code>P1</code> and <code>P2</code>.  Hence, if <code>P1</code> forwards to A
and <code>P1</code> forwards to B then <code>P1 + P2</code> makes a copy of the
input packet and forwards to both A and B.

With this in mind, let's modify the port mapper to inspect the packets both
before and after the rewriting.  We can leave the mapper component unchanged
and add monitoring to the forwarder.
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

Another useful query measures the load at different places in the network.  The
<code>monitorLoad( n , label )</code> policy prints the number of packets and
bytes it receives every <code>n</code> seconds.  Output is prefixed by the
string <code>label</code>, and we can restrict the packets monitored by
<code>monitorLoad</code> using <code>if</code>-<code>then</code> clauses.

Try removing the packet monitoring policies in the port mapper and adding a
<code>monitorLoad</code> query to measure the number of packets sent by iperf.
The implementation of <code>monitorLoad</code> is far more efficient than
<code>monitorPackets</code>, as it does not send packets to the controller.
Instead, it queries OpenFlow counters on the switch after each time interval.
You can issue a longer iperf request by adjusting the timing parameter
(<code>-t seconds</code>).  Watch the load printed in the controller window.
The following command runs iperf for <code>20</code> seconds.
```
mininet> h1 iperf -c 10.0.0.2 -p 5022 -t 20
```

Under the Hood
--------------

Sequential and parallel composition make it easier to write SDN controller
programs, but it all gets compiled to OpenFlow rules in the end.  To get a feel
for how the compiler works, let's take another look at a NetCore version of the
efficient firewall from the [OxFirewall](03-OxFirewall.md) chapter, altering it
slightly to block SSH rather than ICMP traffic:
```
let firewall = if !(tcpDstPort = 22) then all in
monitorTable(1, firewall)
```

We added a table query to show the flow table that NetCore produces for the
firewall.  Now, fire up the firewall
([Ox_Firewall.nc](netcore-tutorial-code/Ox_Firewall.nc)).  You should see the
following output:
```
$ frenetic netcore-tutorial-code/Ox_Firewall.nc
Flow table at switch 1 is:
 {dlTyp = ip, nwProto = tcp, tpDst = 22} => []
 {*} => [Output AllPorts]
 {*} => [Output AllPorts]
```

As you can see from the latter two rules, NetCore is less efficient than a
human programmer.  (But not for long, we hope!)  Nevertheless, this should look
very similar to the flow table you programmed.

Now, let's add a query to monitor traffic from H1:
```
let firewall = if !(tcpDstPort = 22) then all in
let monitor = if srcIP = 10.0.0.1 then monitorLoad(10, "From H1") in
monitorTable(1, firewall + monitor)
```

The flow table should look something like this:
```
$ frenetic netcore-tutorial-code/Ox_Firewall_Monitor.nc
Flow table at switch 1 is:
 {dlTyp = ip, nwSrc = 10.0.0.1, nwProto = tcp, tpDst = 22} => []
 {dlTyp = ip, nwProto = tcp, tpDst = 22} => []
 {dlTyp = ip, nwSrc = 10.0.0.1} => [Output AllPorts]
 {*} => [Output AllPorts]
```

Let's break it down, rule by rule:
* **Rule 1**: drop SSH traffic from H1.
* **Rule 2**: drop SSH traffic.
* **Rule 3**: forward traffic from H1.
* **Rule 4**: forward all other traffic.

Why so many rules?  OpenFlow switches can only count packets as they match a
rule.  Rule 1, for example, is necessary to precisely count traffic from H1.
Without it, our query would miss any SSH traffic sent from H1, as it would be
lumped in with all the other SSH traffic dropped by rule 2.

In general, NetCore creates a flow table for two policies joined by parallel
composition (<code>P1 + P2</code>) by creating flow tables for <code>P1</code>
and <code>P2</code>, and taking the Cartesian product of these tables, and then
concatenating the original tables.  The result looks like this:

![Parallel composition.][parallel_composition]

Section **A** in the flow table is the Cartesian product.  Packets that match
both <code>P1</code> and <code>P2</code> are matched here.  Because **A** is
given a higher priority, any packets that reach **B** or **C** may match
<code>P1</code> *or* <code>P2</code>, but not both.

As an aside, NetCore policies are total functions: they always process every
packet, even if that "processing" is simply to drop it.  Hence, NetCore adds a
final, catch-all rule to the flow table to drop packets that are not matched
higher up.

Programming Exercise: A Multi-Switch Network
---------------------------------------------------------

In this exercise, you will explore how to construct a policy for
a multi-switch network using the composition operators discussed
in this chapter.  You should also practice using queries to help
debug your program.  The network under consideration has 3 switches, 
and one host attached to each switch: 

![Simple linear topology.][topo_2]

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
See anything interesting on h3?  Make sure h3's web server is inaccessible 
from from h2:
```
mininet> h2 wget -O - h3
```

[topo_1]: images/topo_1.png "Default Mininet topology."
[topo_2]: images/topo_2.png "Simple linear topology."
[topo_3]: images/topo_3.png "Simple tree topology."
[parallel_composition]: images/parallel_composition.png "Parallel composition."
