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

<h3 id="Exercise1">Exercise 1: A Naive Repeater</h3>

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

module Controller = Ox_Controller.Make (MyApplication)
```

> Define Ox_Defaults

From the terminal, compile the program as follows:

```shell
$ ocamlbuild -use-ocamlfind -package OxLib ex1.d.byte
```

> Fold PacketLib and OpenFlowLib into OxLib, IMHO.

The `packet_in` function above receives a [PacketIn] message and emits
a [PacketOut] message using [send_packet_out] [OxPlatform]. Note that
the list of actions is empty (`packetOut.actions = []`), which means the
packet is dropped.

#### Programming Task

Instead of dropping the packet, send it out of all
ports (excluding the input port). This is easier than it sounds,
because you can do it with just one [OpenFlow action] [Action]. Once
you've found the right action to apply, rebuild the controller and
test that it works.

#### Testing your Controller

- Start your controller by running:

  ```
  $ ./ex1.d.byte
  ```

- In a separate terminal window, start the Mininet network simulator:

  ```
  $ sudo ./mn --controller=remote --topo=single,3 --mac
  ```

  A brief explanation of the flags:

  * `topo=single` creates a network with one switch and three hosts.

  * `--mac` sets the hosts mac addresses to `1`, `2`, and `3` (instead
    of a random number).

  * `--controller=remote` directs the switches to connect to your controller
    (instead of using a default controller that is built into Mininet).

- On the controller terminal, you should see the following

  ```
  $ ./ex1.d.byte
  Switch 1 Connected.
  ```
  
- On the Mininet terminal, make `h1` ping `h2`:

  ```
  mininet> h1 ping -c 1 h2
  ```

  The command should succeed and print the following:
  ```
  1 packets transmitted, 1 packets received, 0.0% packet loss
  ```
  
- On the controller terminal, you should see the following:
  ```
  ping request
  ping reply
  ```

  > Fill in the action output

  This indicates that the controller itself received the the packets, which
  is not very efficient.
  
### Exercise 2: An Efficient Repeater

Diverting all packets to the controller is very inefficent. You will
now add rules to the switch _flow table_ so that the switch can
process packets locally without sending them to the controller.

*Note*: You still need the packet-in function you wrote above. While
is flow table is being configured (e.g., when the switch is rebooted)
packets may still be diverted to the controller, where they have to be
processed by the packet-in function.

#### Programming Task

You will now fill in the `switch_connected` handler in your program.
Use the following as a template:

```ocaml
let switch_connected (sw : switchId) : unit =
  Printf.printf "Switch %Ld connected.\n%!" sw;
  send_flow_mod sw 1l (FlowMod.add_flow priority pattern action_list)
```

This function uses `send_flow_mod` [OxPlatform] to add a new rule to
the flow table. Your task is to fill in `priority`, `pattern`, and
`action_list`.

- `pattern` is an [OpenFlow pattern] [Match] for matching packets.  Since your
    repeater matches all packets, you can use `Match.all`.

- `priority` is a 16-bit priority for the rule. Since you just have one
  rule, the priority you pick is not relevant, you can just use `0`.

- For `action_list`, you must apply the same actions you did in your
  `packet_in` function. (If not, switch and controller will be
  inconsistent.)


#### Building and Testing Your Controller

You can build and test your controller in exactly as you did in
[Exercise 1][Exercise1]. However, during testing, the controller should not
receive any packets.

> TODO(arjun): fast pings to show that packet_ins can still happen?
   
Exercise 2: Firewall
====================

In this exercise, you will compose your repeater with a simple firewall that
blocks ICMP traffic. As a result, `ping`s will be blocked, but other traffic,
such as Web traffic, will still be handled by the repeater.

### The Firewall Function

You will first write the `packet_in` function for the firewall.  After
you've tested it successfully, you'll configure the flow table to
implement the firewall efficiently.

Use the following template:

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
    let payload = pktIn.PacketIn.payload in
    let pkt = Payload.parse payload in
    send_packet_out sw 0l
      { PacketOut.payload = pk.PacketIn.payload;
        PacketOut.port_id = None;
        PacketOut.actions = []
      }
      
end

module Controller = Ox_Controller.Make (MyApplication)
```

This template is similar to template for the repeater. Whereas the
repeater simply emits the payload of the `pktIn` message, the firewall
needs to inspect the payload to determine if it is an ICMP packet is
an ICMP packet.  Ox includes a [parser and packet library] [Packet]
for some common packet formats, including ICMP.

The parser function produces nested records that represent
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
the accessors in the [packet library] [Packet].

For example, `dlTyp pk = 0x800` returns `true` if `pk` is an IP packet
and otherwise false.

#### Building and Testing Your Controller

- Build your controller by running:

  ```shell
  $ ocamlbuild -use-ocamlfind -package OxLib ex3.d.byte
  ```

- Start your controller by running:

  ```
  $ ./ex3.d.byte
  ```

- In a separate terminal window, start the Mininet network simulator, using
  the same parameters you've used before:

  ```
  $ sudo ./mn --controller=remote --topo=single,3 --mac
  ```

- On the controller terminal, you should see the following

  ```
  $ ./ex1.d.byte
  Switch 1 Connected.
  ```
  
- On the Mininet terminal, make `h1` ping `h2` and vice versa:

  ```
  mininet> h1 ping -c 1 h2
  mininet> h2 ping -c 1 h1
  ```

  These command should fail, printing `100.0% packet loss`.


- On the controller terminal, you should see that only ping requests are
  received:
  
  ```
  ping request
  ping reqeust
  ```

  > TODO(arjun): Fill in actual text.

  This indicates that the controller saw the ping request (and dropped it),
  so no ping response was ever seen.

- Although ICMP is blocker, other traffic, such as Web traffic should
  be unaffected. To ensure that this is the case, you can run a web server
  on a host.

  * In mininet, start new terminals for `h1` and `h2`:

    ```
    mininet> h1 xterm &
    mininet> h2 xterm &
    ```

  * In the terminal for `h1` start a local Web server:

    ```
    # cd ~/src/frenetic/guide
    # python -m SimpleHTTPServer
    ```

  * In the terminal for `h2` fetch a web page from `h1`:

    ```
    # curl 10.0.0.1/index.html
    ```

    This command should successfully display the source for the guide.

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

First, write a [pattern][Match] to match ICMP traffic. Patterns in
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

> TODO(arjun): continue below

Exercise 3: Traffic Monitoring
==============================

- Compose the firewall and repeater with a third application: Web
  traffic monitor (count the total number of packets sent to and from
  port 80).

### The Monitoring Function

- Use the following template:

```ocaml
open Ox
open OxPlatform
open OpenFlow0x01
module MyApplication : OXMODULE = struct

  include OxDefaults
  
  let num_http_packets = ref 0

  let switch_connected (sw : switchId) : unit = 
    Printf.printf "Switch %Ld connected.\n%!" sw
      
  let switch_disconnected (sw : switchId) : unit =
    Printf.printf "Switch %Ld disconnected.\n%!" sw

  let is_http_packet (pk : Packet.packet) : bool = 
    (* [FILL IN HERE]: write this predicate *)

  let packet_in_ex2 (sw : switchId) (xid : xid) (pktIn : PacketIn.t) : unit =
    (* [FILL IN HERE]: the packet_in function from exercise 2 *)

  (* [FILL IN HERE] You can use the packet_in function from OxTutorial2. *)
  let packet_in (sw : switchId) (xid : xid) (pktIn : PacketIn.t) : unit =
    packet_in_ex2 sw xid pktIn;
    if is_http_packet (Payload.parse pktIn.PacketIn.payload) then
      begin
        num_http_packets := !num_http_packets + 1;
        Printf.printf "Seen %d HTTP packets.\n%!" !num_http_packets
      end

end

module Controller = Make (MyApplication)
```

#### Testing 

- Same test sequence as above. But, when sending HTTP traffic, the counter
  should be incremented.

- Run a Web server on port 8080. Note that connectivity works, but the
  counter is not incremented.



Exercise 4: Learning Switch
===========================




[Action]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.Action.html

[PacketIn]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.PacketIn.html

[PacketOut]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.PacketOut.html

[OxPlatform]: http://frenetic-lang.github.io/frenetic/docs/Ox_Controller.OxPlatform.html

[Match]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.Match.html

[Packet]: http://frenetic-lang.github.io/frenetic/docs/Packet.html
