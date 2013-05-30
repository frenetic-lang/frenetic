Introduction
============

This tutorial illustrates how to use NetCore to program a distributed
collection of switches running the OpenFlow protocol.  

This is a literate netcore document that can be executed as a program.
As you read this document, we encourage you to try to complete the
example exercises and play with them in mininet.  The last line of the
file defines the main policy that will be executed when this file is
compiled.  To compile a different policy, simply edit the last line,
as indicated by the instructions preceding it.


NetCore Introduction
========================

Describe the basic semantics and concepts.

- located packets
- policies are functions
- basic functions


Static NetCore Programming Examples
===================================

0. Review the topology in tutorial-topo.
  
1. Write a program to route all traffic between hosts 10 and 20.  
Also route all traffic between 30 and 40.

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
======================

- A dynamic program produces a stream of policies.

Dynamic NetCore Programing 1: NAT
================================

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
=====================================

Explain how Mac-Learning works (composition)

1. Keep the NAT from the previous section, but modify the routing policy as follows:
  - Use a static policy to route between 101 and 102.
  - Use mac-learning for all other routing.
2. Inspect the mac-learning table on switch 102.
3. ...

Solutions
=========

1. 

2.

3.

4.

5.

Main Program
============

To execute this document as a NetCore program, do the following:

The following line determines the policy to be executed.  Replace <learn>
with the name of some other policy defined in this file to test it out.

    policy main = learn
