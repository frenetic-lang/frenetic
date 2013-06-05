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


[Action]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.Action.html

[PacketIn]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.PacketIn.html

[PacketOut]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.PacketOut.html

[OxPlatform]: http://frenetic-lang.github.io/frenetic/docs/Ox_Controller.OxPlatform.html

[Match]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.Match.html

[Packet]: http://frenetic-lang.github.io/frenetic/docs/Packet.html
