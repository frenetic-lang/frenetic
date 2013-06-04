Introduction
============

- SDN from 10,000 feet (very, very brief. Link to something else)

  * Centralized controller defines network-wide policy
  
  * Controller is connected to all switches in the network. These switches
    can be _programmed_ to implement the network-wide policy

- OpenFlow is an open protocol for configuring switches and reading switch
  state.

  * Let's the controller add rules the a _flow table_, which determines
    how the switch processes packets.

  * Even let's the switch divert packets to the controller, where you
    can write arbitrary packet-processing code.

- In this tutorial, you will write implement several policies for OpenFlow,
  using the _Ox Controller Platform_

  * Repeater

  * Firewall

  * Network monitor

  * Learning switch

- OCaml. It is amazing. Why are you still using Haskell?

  - We are going to go easy on the OCaml.

  - Ox controllers are simple, event-driven programs.

  - Packet processing logic is very generic. What you learn can be used
    to build controllers for NOX, POX, Beacon, etc.

Handy References
----------------

- _Teach Yourself OCaml in 21 Days_ by Nate Foster and child.

- OpenFlow 1.0 Specification

Exercise 1: Repeater
====================


Exercise 2: Firewall
====================


Exercise 3: Traffic Monitoring
==============================


Exercise 4: Learning Switch
===========================

