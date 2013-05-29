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

5. Construct a firewall for the network.

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