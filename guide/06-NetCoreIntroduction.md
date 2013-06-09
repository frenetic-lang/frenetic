Chapter 6: Introducing NetCore
==============================

- You've learned how to write controllers with OpenFlow. We've shown
  you a simple, two step recipe to implement policies:

  * You can easily write a _packet-in_ function to have the controller
     implement any policy -- though it may be a very inefficient
     implementation

  * You can then use flow tables and statistics to program switches to
    implement the same policy efficiently.

- In the following chapters, we introduce a new way to program SDN policies.

  - You will write policy-functions in a little language we call
    **NetCore**.

  - You will then feed your program to the the NetCore Compiler, which
    synthesizes flow tables that implement your function. (It also
    sends statistics requests, accumulates replies, manages
    switch connections, and more.)

- `guide/netcore-tutorial-code`.

  ```
  $ cd guide/netcore-tutorial-code
  ```

### Example 1: A Naive Repeater (Redux)

In the [OxRepeater](02-OxRepeater.md) chapter, you learned how to program an
efficient repeater by adding rules to the switch flow table.  Recall that a
repeater simply forwards incoming packets out all other ports.

In this example, we will begin by considering a network with just one switch
with two ports, numbered 1 and 2:  

![Default Mininet topology.][topo_1]

Our first goal will be to program a
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
or <code>dlTyp</code>.  Conditions can also be formed using conjunctions
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
find the repeater policy in <code>Repeater.nc</code>.  To start the
repeater controller, just type:
```
$ frenetic Repeater.nc
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

#### Test the Example

At the mininet prompt, test your repeater program by pinging <code>h2</code> from <code>h1</code>:
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
Ping <code>h1</code> from <code>h2</code> as well.
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


Reasoning About NetCore
=======================

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
(<code>dlSrc</code>, <code>dlDst</code>, <code>srcIP</code>, etc.) as well as
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
subsequent number of output packets.  And then apply the topology function T
again, etc.

In summary, one traces the flow of packets through a network by alternately
applying the policy function P and the topology function T. Static NetCore is
just a domain-specific language that makes it easy to write down a 
policy function P that determines how switches forward packets. The main
features of Static NetCore include the following.

  - a set of primitive *actions*, which allow programmers to modify and 
forward packets,
  - *conditional statements*, which allow programmers to perform 
different actions on different kinds of packets or on the same kinds
of packets but at different locations in the network,
  - *sequencing*, which allows programmers to perform a series of
transformations on a packet,
  - *parallel composition*, which allows programmers to make a logical copy
of a packet and thereby to generate more than one result from their
policy --- perhaps forwarding the packet to two different locations, and
  - *queries*, which allow programmers to inspect the contents of packets
as they flow across a network, the load at various points in the network,
the current static policy in force or the concrete rules installed on
OpenFlow flow tables.






## Next chapter: [NetCore Introduction][Ch7]

[Ch7]: 07-NCFirewall.md

[topo_1]: images/topo_1.png "Default Mininet topology."
[topo_2]: images/topo_2.png "Simple linear topology."
[topo_3]: images/topo_3.png "Simple tree topology."
