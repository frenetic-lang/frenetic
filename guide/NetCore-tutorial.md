Frenetic Tutorial
=============

The goal of this tutorial is to teach readers how to program a Software-Defined Network (SDN) running OpenFlow using the Frenetic programming language.  This involves explaining the syntax and semantics of Frenetic and illustrating its use on a number of simple examples.  Along the way, there are a number of exercises for the reader.  Solutions appear at the bottom of the page.

In addition to being a tutorial, this file is also what is known as a [literate program](http://en.wikipedia.org/wiki/Literate_programming). In other words, readers can download and execute this text file as is, without making any modifications.  Intuitively, what we have done is simply turn the commenting conventions on their head:  by default, everything is a comment.  The only regions of the file that will be executed as code are those that are 
indented 4 spaces.

As you read this document, we encourage you to try to complete the example exercises and play with them in mininet.  The last line of this file defines the main Frenetic policy that will be executed when this file is compiled.  To change Frenetic policy that is executed, simply edit the last line.  To compile and execute this file in mininet, please see the instructions at the [end of this document](#compilation_instructions).

Motivation for the Frenetic Design
-----------------------------------------------

*Mostly plagiarized from IEEE overview paper*

Traditional networks are built out of special-purpose devices running distributed protocols that provide functionality such as routing, trafﬁc monitoring, load balancing, NATing and access control. These devices have a tightly-integrated control and data plane, and network operators must separately conﬁgure every protocol on each individual device. This configuration task is a challenging one as network operators must struggle with a host of different baroque, low-level, vendor-specific configuration languages.  Moreover, the pace of innovation is slow as device internals and APIs are often private and proprietary, making it difficult to develop new protocols or functionality to suit client needs.    

Recent years, however, have seen growing interest in software-deﬁned networks (SDNs), in which a logically-centralized controller manages the packet-processing functionality of a distributed collection of switches. SDNs make it possible for programmers to control the behavior of the network directly, by conﬁguring the packet-forwarding rules installed on each switch.  Moreover, the Open Networking Foundation is committed to developing a standard, open, vendor-neutral protocol for controlling collections of switches.  This protocol is OpenFlow.

SDNs can both simplify existing applications and also serve as a platform for developing new ones. For example, to implement shortest-path routing, the controller can calculate the forwarding rules for each switch by running Dijkstra’s algorithm on the graph of the network topology instead of using a more complicated distributed protocol. To conserve energy, the controller can selectively shut down links or even whole switches after directing trafﬁc along other paths. To enforce ﬁne-grained access control policies, the controller can consult an external authentication server and install custom firewall rules.

But although SDNs makes it possible to program the network, they do not make it easy. Protocols such as OpenFlow expose an interface that closely matches the features of the underlying switch hardware. Roughly speaking, OpenFlow allows programmers to manually install and uninstall individual packet-processing rules.  First-generation controller systems such as NOX, Beacon, and Floodlight support the same low-level interface, which forces applications to be implemented using programs that manipulate the fine-grained state of individual devices.  Unfortunately, it is extremely difficult to develop independent program components, such as a router, firewall and network monitor, that collaborate to control the flow of traffic through a network since the application must ultimately install a *single* set of low-level rules on the underlying switches.  This single set of rules must simultaneously implement the desired high-level semantics for each independent high-level component.

In addition, a network is a distributed system, and all of the usual complications arise—in particular, control messages sent to switches are processed asynchronously. Programming asynchronous, distributed systems is notoriously difficult and error prone.  Network programmers require require support to get this right.

The goal of the Frenetic language is to raise the level of abstraction for programming SDNs. To replace the low-level imperative interfaces available today, Frenetic offers a suite of declarative abstractions for querying network state, deﬁning forwarding policies, and updating policies in a consistent way.  These constructs are designed to be *modular* so that individual policies can be written in isolation, by different developers and later composed with other components to create sophisticated policies. This is made possible in part by the design of the constructs themselves, and in part by the underlying run-time system, which implements them by compiling them down to low-level OpenFlow forwarding rules.  Our emphasis on modularity and composition—the foundational principles behind effective design of any complex software system—is the key feature that distinguishes Frenetic from other SDN controllers.

Frenetic Introduction
----------------------

Describe the basic semantics and concepts.

- located packets
- policies are functions
- basic functions


Static NetCore Programming Examples
------------------------------------

0. Review the topology in tutorial-topo.  Define some user-friendly switch names for our topology too:

```
let A : switch = 101
let B : switch = 102
let C : switch = 103
let D : switch = 104
```

1. Write a program to route all ip traffic as follows: 
  - packets with destination ip 10.0.0.10 arriving at switch C go to host 10.  
  - packets with destination ip 10.0.0.20 arriving at switch C go to host 20.  
  - packets with destination ip 10.0.0.30 arriving at switch D go to host 30.  
  - packets with destination ip 10.0.0.40 arriving at switch D go to host 40.  
  - flood all arp packets arriving at any switch
Try out your program by pinging 10 from 20 and 20 from 10.  What happens?
What happens if you ping 10 from 30?

Start your work with this handy wrapper to handle flooding of arp packets.
Replace "drop" below with some other policy that solves the problem.

```
let arpify (P:policy) =
  if frameType = arp then all
  else P
  
let my_sol1 = arpify(drop)
```

2. Extend the program written in (a) to route all traffic between
10, 20, 30, 40.

3. Now consider the program that routes all traffic between all hosts,
which we will provide.

4. Use the NetCore query facility to solve the following problem:
Discover the set of all TCP ports being used by either machines 10 or
20.  Here is a [list of common port
numbers](http://packetlife.net/media/library/23/common-ports.pdf)
Note: some protocols are sending a lot of traffic (eg: HTTP, on port
80).  As you investigate, narrow your searches to find the "needle in
the haystack."  There is one host sending a small amount of traffic to
a non-standard port in a nonstandard format.  Which host is it?  Print
the packets using that port.  They contain a secret message.

5. Construct a firewall for the network that enforces the following policy.
Compose it with the routing policy defined in part 3.
  - Machines 10, 20, 30, 40 are trusted machines.  
  - Machine 50 is an untrusted machine.  
  - Each of these machines is serving files using HTTP (TCP port 80).  
  - Machines 10 and 20 have private files that may not be read by untrusted machines.  
  - Machines 30, 40, 50 have public files that may be read by any machine.
  - Machine 50 may not ssh (TCP port 22) in to any of the private machines
  - All other traffic must be allowed to pass through the network.

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

```
mininet> h50 cat /usr/share/dict/words | nc -l 80 &
mininet> h10 curl 10.0.0.50 # should print words
```
 
4. Determine the public and private ports that this connection uses, using monitor_tbl / monitor_pol

5. Run a Web server on h30:
```
mininet> h30 cat /usr/share/dict/words | nc -l 80 &
```

6. Connect to the Web server from h10 to ensure that it is working:

```
mininet> h10 curl 10.0.0.30 # should print words
```

7. Can you connect from h50? Why not?

```
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

Solutions
---------

### Static Solution 1

```
let deliver(s:switch, i:ip, p:port) =
  if switch = s && dstip = i then fwd p 

let routing1_for_C = 
  deliver(C, 10.0.0.10, 2) + 
  deliver(C, 10.0.0.20, 3)
  
let routing1_for_D =
  deliver(D, 10.0.0.30, 2) +
  deliver(D, 10.0.0.40, 3)

let sol1 = 
  arpify(routing1_for_C + routing1_for_D)
```    
  

### Static Solution 2

```
let routing2_for_C =
  deliver(C, 10.0.0.10, 2) + 
  deliver(C, 10.0.0.20, 3) +
  deliver(C, 10.0.0.30, 1) +
  deliver(C, 10.0.0.40, 1)
  
let routing2_for_B =
  deliver(B, 10.0.0.10, 2) + 
  deliver(B, 10.0.0.20, 2) +
  deliver(B, 10.0.0.30, 3) +
  deliver(B, 10.0.0.40, 3)
  
let routing2_for_D =
  deliver(B, 10.0.0.10, 1) + 
  deliver(B, 10.0.0.20, 1) +
  deliver(B, 10.0.0.30, 2) +
  deliver(B, 10.0.0.40, 3)
  
let sol2 =
  (routing2_for_B + routing2_for_C + routing2_for_D)
```

### Static Solution 3

```
let routing3_for_C =
  deliver(C, 10.0.0.10, 2) + 
  deliver(C, 10.0.0.20, 3) +
  deliver(C, 10.0.0.30, 1) +
  deliver(C, 10.0.0.40, 1) +
  deliver(C, 10.0.0.50, 1)
  
let routing3_for_B =
  deliver(B, 10.0.0.10, 2) + 
  deliver(B, 10.0.0.20, 2) +
  deliver(B, 10.0.0.30, 3) +
  deliver(B, 10.0.0.40, 3) +
  deliver(B, 10.0.0.50, 1)
  
let routing3_for_D =
  deliver(B, 10.0.0.10, 1) + 
  deliver(B, 10.0.0.20, 1) +
  deliver(B, 10.0.0.30, 2) +
  deliver(B, 10.0.0.40, 3) +
  deliver(B, 10.0.0.50, 1)
  
let routing3_for_A =
  if switch = A then
    if srcip = 10.0.0.5 then fwd 1
    else fwd 2
  
let sol3 = 
  arpify (routing3_for_A + routing3_for_B + routing3_for_C + routing3_for_D)
```

### Static Solution 4

Hosts 10, 20, 30, 40 and 50 are all sending large amounts of http traffic (port 80) and some ssh traffic (port 22).
Host 50 sends some special traffic on port 6110.  You might discover the existence of that traffic by writing a
query that filters out the http and ssh traffic, so that system only prints the 6110 traffic:

```
let monitor =
  if !(tcpport = 80 || tcpport = 22) then monitor_sw()  

let sol4 =
  sol3 + monitor
```

### Static Solution 5

Compilation Instructions
=========================

To execute this document as a NetCore program, do the following:

The following line determines the policy to be executed.  Replace **learn**
with the name of some other policy defined in this file to test it out.

```
policy main = learn
```
