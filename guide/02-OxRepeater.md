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

[Action]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.Action.html

[PacketIn]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.PacketIn.html

[PacketOut]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.PacketOut.html

[OxPlatform]: http://frenetic-lang.github.io/frenetic/docs/Ox_Controller.OxPlatform.html

[Match]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.Match.html

[Packet]: http://frenetic-lang.github.io/frenetic/docs/Packet.html
