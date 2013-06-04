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

- The Ox platform provides several functions to send different types
  of messages to switches. In turn, your Ox application must define
  event handlers to receive messages from switches. In this tutorial,
  we will eventually cover all these messages.

- See *Ox reference manual* for a overview.

- In this exercise, we will build a repeater: an network element that
  forwards incoming packets on all other ports. This is a very naive
  way to get connectivity in a network, but it is a good warmup.

- We will learn how to configure the _flow table_ on an OpenFlow
  switch to implement a policy efficientlhy.

- We will learn how to process packets on the controller itself. This
  is much slower than packet-processing on a switch. But, there are
  cases where it is necessary.

<h3>Part 1: Repeater on the Controller</h3>

- We eschew the flow table and handle all packets at the controller.

- In OpenFlow, if the flow table doesn't match a packet, the packet is
  sent to the controller.

- When the switch is rebooted, its flow table is empty and all packets
  are divereted to the controller instead (to the `packet_in`
  handler).

- In this exercise, we write a trivial repeater that processes all
  packets in the `packet_in` event handler.

- Open the file `OxTutorial1.ml`. There are dummy event handlers that
  ignore all events they receive.

- However, the `packet_in` handler prints the packets it receives and
  then drops calling `send_packet_out`, which the Ox platform
  provides.

- Please see section [FILL] of the OpenFlow specification for a
  comprehensive explanation of `PacketIn` and `PacketOut` messages,
  their fields, etc.

- Here is a trivial `packet_in` handler that simply drops all packets
  it receives:

```ocaml
let packet_in (sw : switchId) (xid : xid) (pktIn : PacketIn.t) : unit =
  Printf.printf "Received a packet from %Ld.\n%!" sw;
  send_packet_out sw 0l
    { PacketOut.payload = pktIn.PacketIn.payload;
      PacketOut.port_id = None;
      PacketOut.actions = []
    }
```
- `pktIn` is a record representing the PacketIn message. Its most
  significant field is `pktIn.PacketIn.payload`, which is the packet
  that was received. It has few other fields with additional metadata
  about the packet. See [REF] for details.

- `send_packet_out` can be used to sent any payload to a switch. Here,
  we simply re-send the packet we received (as `PacketOut.payload`)

- `PacketOut.actions` specifies a list of actions to apply the
  packet. The actions of OpenFlow 1.0 allow several headers to be
  modified and packets to be emitted.

- Above, `PacketOut.actions` is empty, so the packet is dropped.

- *Programming Task*: instead of dropping the packet, send it out of
  all ports, but not the packet's input port.

  * You do this by editing the action list `PacketOut.action = []`.

  * See [REF], which lists all the actions that OpenFlow supports.

    > Arjun: this is just to force people to read this bit of the manual.

  * Build your controller by typing `make` in the `OxTutorial1` directory.

- *Testing Your Program*

  * Start your controller by running:

    ```
    $ ./controller
    ```

  * In a separate terminal window, start the Mininet network simulator:

    ```
    $ ./mininet
    ```

  * This script create a virtual network with one switch (`s1`) and
    two hosts (`h1` and `h2`). Test it by pinging between both hosts:

    ```
    mininet> h1 ping h2
    ```

  * If you look at the terminal for your controller, you'll see that it
    receives all ICMP packets itself.

<h3>Part 2: An Efficient Repeater</h3>

- Diverting all packets to the controller is very inefficent. You will
  now add rules into the switch's _flow table_ so that the switch can
  process packets locally without sending them to the controller.

- *Note*: You still need the packet-in function you wrote above. While is
  flow table is being configured (e.g., when the switch is rebooted) packets
  may still be diverted to the controller, where they have to be processed
  by the packet-in function.

- You will now fill in the `switch_connected` handler in your program.
  When the switch first connects, use `send_flow_mod` [REF] as follows:

  ```ocaml
   let switch_connected (sw : switchId) : unit =
     Printf.printf "Switch %Ld connected.\n%!" sw;
     send_flow_mod sw 1l (FlowMod.add_flow priority pattern action_list)
  ```

  Fill in `priority`, `pattern`, and `action_list`.

  * `pattern` is an OpenFlow pattern for matching packets.
    Since your repeater matches all packets, use `Match.all`

  * `priority` is a 16-bit priority for the rule. `65536` is the highest
    priority and `0` is the lowest priority.

  * For `action_list`, you must apply the same actions you did in your
    `packet_in` function. (If not, switch and controller will be
    inconsistent.)

   * Testing as above, but pings should not reach the switch.


      (FlowMod.add_flow 200 Match.all [Action.Output PseudoPort.AllPorts])

   * TODO(arjun): fast pings to show that packet_ins can still happen?
   
Exercise 2: Firewall
====================

In this exercise, you will compose your repeater with a simple firewall that
blocks ICMP traffic. As a result, `ping`s will be blocked, but other traffic,
such as Web traffic, will still be handled by the repeater.

<h3>A Naive Firewall</h3>

We will start by writing the `packet_in` function for this
policy. After we've successfully tested the `packet_in` function,
we'll build an efficient firewall that operates locally on the switch.

The repeater simply emits the payload of the `pktIn` message. In
contrast, your firewall has to inspect the payload and determine if it
is an ICMP packet.  Ox includes a parser for some common packet
formats, including ICMP.

```ocaml
let packet_in (sw : switchId) (xid : xid) (pktIn : PacketIn.t) : unit =
  let payload = pktIn.PacketIn.payload in
  let pkt = Payload.parse payload in
  ...
```

The `Payload.parse` function produces nested records that represent
the logical structure of the payload. For example, a ping request
would be represented as:

```ocaml
{ dlSrc = 0x000000000001; (* source mac address *)
  dlDst = 0x000000000002; (* destination mac address *)
  dlVlan = None;
  dlVlanPcp = 0;
  nw = Ip { Ip.Icmp { Icmp.typ = 8; (* echo request *)
                      Icmp.code = 0;
                      Icmp.chksum = ...;
                      Icmp.payload = ... } } }
```

Instead of navigating nested records such as these, we recommend using
the predicates ([REF]) that Ox provides.

- Predicate tips

- Testing

<h3>An Efficient Firewall</h3>



Exercise 3: Traffic Monitoring
==============================


Exercise 4: Learning Switch
===========================

