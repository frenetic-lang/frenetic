Frenetic Tutorial
=============

*DPW: This preamble is old and needs revision*

*DPW TODO:  I called everything "Frenetic" or "Static Frenetic" 
but that was a bad idea.  Should revise to call NetCore NetCore and 
Frenetic Frenetic.*

The goal of this tutorial is to teach readers how to program a Software-Defined Network (SDN) running OpenFlow using the Frenetic programming language.  This involves explaining the syntax and semantics of Frenetic and illustrating its use on a number of simple examples.  Along the way, there are a number of exercises for the reader.  Solutions appear at the bottom of the page.

In addition to being a tutorial, this file is also what is known as a [literate program](http://en.wikipedia.org/wiki/Literate_programming). In other words, readers can download and execute this text file as is, without making any modifications.  Intuitively, what we have done is simply turn the commenting conventions on their head:  by default, everything is a comment.  The only regions of the file that will be executed as code are those that are 
indented 4 spaces.

As you read this document, we encourage you to try to complete the example exercises and play with them in mininet.  The last line of this file defines the main Frenetic policy that will be executed when this file is compiled.  To change Frenetic policy that is executed, simply edit the last line.  To compile and execute this file in mininet, please see the instructions at the [end of this document](#compilation_instructions).

Motivation for the Frenetic Design
-----------------------------------------------

*Much in this section was plagiarized from IEEE overview paper*

Traditional networks are built out of special-purpose devices running distributed protocols that provide functionality such as routing, trafﬁc monitoring, load balancing, NATing and access control. These devices have a tightly-integrated control and data plane, and network operators must separately conﬁgure every protocol on each individual device. This configuration task is a challenging one as network operators must struggle with a host of different baroque, low-level, vendor-specific configuration languages.  Moreover, the pace of innovation is slow as device internals and APIs are often private and proprietary, making it difficult to develop new protocols or functionality to suit client needs.    

Recent years, however, have seen growing interest in software-deﬁned networks (SDNs), in which a logically-centralized controller manages the packet-processing functionality of a distributed collection of switches. SDNs make it possible for programmers to control the behavior of the network directly, by conﬁguring the packet-forwarding rules installed on each switch.  Moreover, the Open Networking Foundation is committed to developing a standard, open, vendor-neutral protocol for controlling collections of switches.  This protocol is OpenFlow.

SDNs can both simplify existing applications and also serve as a platform for developing new ones. For example, to implement shortest-path routing, the controller can calculate the forwarding rules for each switch by running Dijkstra’s algorithm on the graph of the network topology instead of using a more complicated distributed protocol. To conserve energy, the controller can selectively shut down links or even whole switches after directing trafﬁc along other paths. To enforce ﬁne-grained access control policies, the controller can consult an external authentication server and install custom firewall rules.

But although SDNs makes it possible to program the network, they do not make it easy. Protocols such as OpenFlow expose an interface that closely matches the features of the underlying switch hardware. Roughly speaking, OpenFlow allows programmers to manually install and uninstall individual packet-processing rules.  First-generation controller systems such as NOX, Beacon, and Floodlight support the same low-level interface, which forces applications to be implemented using programs that manipulate the fine-grained state of individual devices.  Unfortunately, it is extremely difficult to develop independent program components, such as a router, firewall and network monitor, that collaborate to control the flow of traffic through a network since the application must ultimately install a *single* set of low-level rules on the underlying switches.  This single set of rules must simultaneously implement the desired high-level semantics for each independent high-level component.

In addition, a network is a distributed system, and all of the usual complications arise—in particular, control messages sent to switches are processed asynchronously. Programming asynchronous, distributed systems is notoriously difficult and error prone.  Network programmers require require support to get this right.

The goal of the Frenetic language is to raise the level of abstraction for programming SDNs. To replace the low-level imperative interfaces available today, Frenetic offers a suite of declarative abstractions for querying network state, deﬁning forwarding policies, and updating policies in a consistent way.  These constructs are designed to be *modular* so that individual policies can be written in isolation, by different developers and later composed with other components to create sophisticated policies. This is made possible in part by the design of the constructs themselves, and in part by the underlying run-time system, which implements them by compiling them down to low-level OpenFlow forwarding rules.  Our emphasis on modularity and composition—the foundational principles behind effective design of any complex software system—is the key feature that distinguishes Frenetic from other SDN controllers.

Preliminary Programming Concepts
--------------------------------

A Frenetic policy describes how a collection of switches
forwards packets from one location to another.  We call a Frenetic policy
*static* when it is fixed ahead of time,
does not change, and does not depend upon the packets flows that
appear in the network.  We will focus first on static policies 
and add some simple dynamics later in the tutorial.

The Frenetic programming paradigm encourages users to think of static
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
Keep in mind that all packets processed by Frenetic come with associated
location information.

To understand how a packet flows through a network, a programmer must
analyze both the current Frenetic policy P and the network topology
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
Static Frenetic is just a domain-specific
language that makes it easy to write down a single policy function
P that determines how switches forward packets.

Introductory Examples
---------------------

The main features of Static Frenetic include the following.

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
let repeater =
  if inPort = 1 then fwd(2)
  else fwd(1)
```

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
At the mininet prompt, test your repeater program using ping:

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

### Example 2: Simple Port Translation

### Example 3: A Larger Network

### Example 4: Queries and Debugging 


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
