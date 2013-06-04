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

- The Ox platform provides several functions to send different types of messages to switches. In turn, your Ox application must define event handlers to receive messages from switches. In this tutorial, we will eventually cover all these messages.

- See *Ox reference manual* for a overview.

- In this exercise, we will build a repeater: an network element that forwards incoming packets on all other ports. This is a very naive way to get connectivity in a network, but it is a good warmup.

-  We will learn how to configure the  _flow table_ on an OpenFlow switch to implement a policy efficientlhy.

-  We will learn how to process packets on the controller itself. This is much slower than packet-processing 
   on a switch. But, there are cases where it is necessary.

<h3>Part 1: Repeater on the Controller</h3>

- We eschew the flow table and handle all packets at the controller.
- In OpenFlow, if the flow table doesn't match a packet, the packet is sent to the controller.
- When the switch is rebooted, its flow table is empty and all packets are divereted to the controller instead
  (to the `packet_in` handler).

- In this exercise, we write a trivial repeater that processes all packets in the `packet_in` event handler.

- Open the file `OxTutorial1.ml`. There are dummy event handlers that ignore all events they receive.

- However, the `packet_in` handler prints the packets it receives and then drops calling `send_packet_out`, which
  the Ox platform provides.

- Please see section [FILL] of the OpenFlow specification for a comprehensive explanation of `PacketIn` and `PacketOut` messages, their fields, etc.

- Here is a trivial `packet_in` handler that simply drops all packets it receives:

```ocaml
let packet_in (sw : switchId) (xid : xid) (pktIn : PacketIn.t) : unit =
  Printf.printf "Received a packet from %Ld.\n%!" sw;
  send_packet_out sw 0l
    { PacketOut.payload = pktIn.PacketIn.payload;
      PacketOut.port_id = None;
      PacketOut.actions = []
    }
```
- `pktIn` is a record representing the PacketIn message. Its most significant field is
  `pktIn.PacketIn.payload`, which is the packet that was received. It has few other fields with additional
  metadata about the packet. See [REF] for details.

- `send_packet_out` can be used to sent any payload to a switch. Here, we simply re-send the packet we received
  (as `PacketOut.payload`)

- `PacketOut.actions` specifies a list of actions to apply the packet. The actions of OpenFlow 1.0 allow several headers to be modified and packets to be emitted.

- Above, `PacketOut.actions` is empty, so the packet is dropped.

[FILL]

Exercise 1: Repeater
====================


Exercise 2: Firewall
====================


Exercise 3: Traffic Monitoring
==============================


Exercise 4: Learning Switch
===========================

