Getting Started
===============

In this tutorial, you will learn to program software-defined networks
(SDN) using OpenFlow. The software for this tutorial is available as
a virtual machine. To get started:

- Download and install the [VirtualBox](https://www.virtualbox.org)
  virtualization platform.
  
- Download the
  [Frenetic Tutorial VM](http://www.cs.brown.edu/~arjun/tmp/Frenetic.vdi).

  > Insert the right link.

- Launch the tutorial VM, which will launch a GUI and automatically login
  as `frenetic`. The password for the account is also `frenetic`.

- At a terminal, go to the tutorial directory, check for updates, and
  rebuild the tutorial software:

  ```
  $ cd src/frenetic
  $ git pull
  $ make reinstall
  ```

  > Make `make reinstall` work.

Handy References
----------------

- [Introduction to OCaml](http://www.cs.cornell.edu/courses/cs3110/2012fa/recitations/rec01.html)

  In this tutorial, you will build controllers in OCaml. We use a tiny
  fragment of the language and provide several examples, but a little
  familiarity with OCaml syntax will be helpful.

  We recommend that you either (1) skim the Introduction to OCaml, or
  (2) do this tutorial with a partner who has passing familiarity with
  OCaml (or Haskell, or some related language).

  > Pick a good introduction. I think the 3110 intro has too many words.

- [Ox Platform Reference](http://frenetic-lang.github.io/frenetic/docs/)
  
  You will write your controllers using Ox, which is a lightweight
  library for writing controllers in OCaml. This tutorial will guide you
  though writing Ox controllers.

  Ox is loosely based on controllers such as [POX]
  (https://openflow.stanford.edu/display/ONL/POX+Wiki) and [NOX]
  (http://www.noxrepo.org/nox/about-nox/). You should be able to apply
  what you learn in this tutorial to write POX/NOX controllers too.

- [OpenFlow 1.0 Specification] (http://www.openflow.org/documents/openflow-spec-v1.0.0.pdf)

  The OpenFlow specification describes OpenFlow-conformant switches
  and details the wire-format of the OpenFlow protocol. You'll find that
  most of the Ox Platform Reference simply reflects the OpenFlow messages
  and data types into OCaml.

- [Mininet] (http://mininet.org/walkthrough/)

  You will use the Mininet network simulator to run your
  controllers. We will tell you exactly what Mininet commands to use,
  so you don't need to read this.

Exercise 1: Repeater
====================

### OpenFlow Overview

In a basic SDN, all switches connect to a centralized controller
machine. The controller thus has a global view of network, and can
_program all switches_ to implement a unified, network-wide policy.
To program a switch, the controller uses a standard protocol, such as
OpenFlow.

A switch processes packets using a _flow table_, which is a list of
prioritized packet-processing rules.  Each rule has a pattern (to
match packets), a list of actions (to apply to matching packets), and
various counters that collect statistics on processed traffic. If a
pattern matches several packets, the switch applies the
highest-priority rule.

For example, the following is a sketch of a flow table that drops ICMP
traffic, floods TCP traffic, and sends all other traffic to the controller:

<table>
<tr>
  <th>Priority</th>
  <th>Pattern</th>
  <th>Action</th>
  <th>Counter (bytes)</th>
</tr>
<tr>
  <td>50</td>
  <td>ICMP</td>
  <td>drop</td>
  <td>50</td>
</tr>
  <td>40</td>
  <td>TCP</td>
  <td>flood</td>
  <td>700</td>
</tr>
<tr>
  <td>40</td>
  <td>UDP</td>
  <td>controller</td>
  <td>50</td>
</tr>
</table>

By sending packets to the controller, the controller can implement an
arbitrary packet-processing function (e.g., deep-packet inspection).
Sending packets to the controller is much slower than processing them
locally on a switch.  So, a controller typically inserts rules into
the flow table to implement the packet-processing function efficiently.

### Exercise 1: A Naive Repeater

As a warmup exercise, you will build a repeater that forwards incoming
packets on all other ports. You will do so in two steps. First, you
will leave the flow table empty, so all packets are diverted to the
controller for processing.  After you complete and test that this
naive strategy works correctly, you'll insert rules into the flow
table to process packets on the switch itself.

The Ox platform provides several functions to send different types of
messages to switches. In turn, your Ox application must define event
handlers to receive messages from switches. In this tutorial, we will
eventually cover all these messages.

For now, you only need to write a handler for the `packet_in` message.
Create a file `ex1.ml` and fill it with the following template:

```ocaml
module MyApplication : Ox_Controller.OXMODULE = struct
  open Ox_Controller.OxPlatform
  open OpenFlow0x01
  
  include Ox_Defaults

  let switch_connected (sw : switchId) : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw

  let switch_disconnected (sw : switchId) : unit =
    Printf.printf "Switch %Ld disconnected.\n%!" sw

  let packet_in (sw : switchId) (xid : xid) (pk : PacketIn.t) : unit =
    Printf.printf "%s\n%!" (PacketIn.to_string pk);
    send_packet_out sw 0l
      { PacketOut.payload = pk.PacketIn.payload;
        PacketOut.port_id = None;
        PacketOut.actions = []
      }

end
```

> Define Ox_Defaults

From the terminal, compile the program as follows:

```shell
$ ocamlbuild -use-ocamlfind -package OxLib ex1.d.byte
```

> Fold PacketLib and OpenFlowLib into OxLib, IMHO.

The `packet_in` function above receives a [PacketIn] message and emits
a [PacketOut] message using [send_packet_out] [OxPlatform].

[PacketIn]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.PacketIn.html

[PacketOut]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.PacketOut.html

[OxPlatform]: http://frenetic-lang.github.io/frenetic/docs/Ox_Controller.OxPlatform.html


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

### The Firewall Function

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

### An Efficient Firewall

In this part, you will implement the firewall efficiently, using the
flow table on the switch. You still need a `packet_in` function to
process packets sent before the flow table is initialized. You should
simply build on your solution to the previous part and _leave its
`packet_in` function untouched_.

Fill in the `switch_connected` event handler. You need to install two
entries into the flow table--one for ICMP traffic and the other for
all other traffic;

```ocaml
let switch_connected (sw : switchId) : unit =
  Printf.printf "Switch %Ld connected.\n%!" sw;
  send_flow_mod sw 0l (FlowMod.add_flow prio1 pat1 actions1);
  send_flow_mod sw 0l (FlowMod.add_flow prio2 pat2 actions2)
```

Your task is to fill in the priorities, patterns, and actions in the
handler above.

First, write an OpenFlow pattern to match ICMP traffic. Patterns in
OpenFlow 1.0 can match the values of 12 pre-determined packet headers.
For example, the following pattern matches all traffic from the host
whose Ethernet address is `00:00:00:00:00:12`:

```ocaml
let from_host12 =
  let open Match in
  { dlSrc = Some 0x000000000012L; (* the L suffix indicates a long integer *)
    dlDst = None; 
    dlTyp = None;
    dlVlan = None;
    dlVlanPcp = None;
    nwSrc = None;
    nwDst = None;
    nwProto = None;
    nwTos = None;
    tpSrc = None;
    tpDst = None;
    inPort = None }
```

In a pattern, `header = Some x` means that the value of `header` must be `x`
and `header = None` means that `header` may have any value.

```ocaml
let from_10_0_0_1 = 
  let open Match in
  { dlSrc = None;
    dlDst = None; 
    dlTyp = 0x800; (* frame type for IP *)
    dlVlan = None;
    dlVlanPcp = None;
    nwSrc = 0x10000001; (* 10.0.0.1 *)
    nwDst = None;
    nwProto = None;
    nwTos = None;
    tpSrc = None;
    tpDst = None;
    inPort = None }
```

This pattern also specifies the frame type for IP packets (`dlTyp =
0x800`). If you don't write the frame type, the value of `nwSrc` is
ignored.

When the switch processes a packet, it applies the actions from _the
highest-priority matching entry_. If a packet matches several entries
with the same priority, the behavior is unspecified. Therefore, pick
different priorities for each pattern, unless you are certain the
patterns are disjoint.


Exercise 3: Traffic Monitoring
==============================


Exercise 4: Learning Switch
===========================

