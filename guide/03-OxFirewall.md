Chapter 2: Firewall
===================

In this chapter, you will compose your repeater with a simple firewall
that blocks ICMP traffic. As a result, `ping`s will be blocked, but
other traffic, such as Web traffic, will still be handled by the
repeater.

### The Firewall Function

You will first write the `packet_in` function for the firewall:

```ocaml
let packet_in (sw : switchId) (xid : xid) (pktIn : packetIn) : unit =
  ...
```

After you've tested it successfully, you'll configure the flow table
to implement the firewall efficiently. Unlike the repeater, which
blindly forwards packets, the firewall needs inspect packet-headers to
determine if they should be dropped. The argument `pktIn` of type
[packetIn] has a binary payload that holds the headers you need.

Ox includes a packet parsing library that supports several common formats,
including ICMP. You can use it parse the packet as follows:

```ocaml
let packet_in (sw : switchId) (xid : xid) (pktIn : packetIn) : unit =
  let payload = pktIn.input_payload in
  let pk = parse_payload payload in
  ...
```

Applying `parse_payload` will parse the packet into a series of nested
frames. You can extract headers from these frames using OCaml's
pattern matching (not recommended) or using the [accessor functions] in
the packet library, which is what we recommend.

#### Programming Task

Starting from [./ox-tutorial-code/Firewall.ml], fill in the
`is_icmp_packet` function.

Recall that ICMP packets are contained in IP (frame type 0x800) and have
the protocol number 1. In addition, note taht it doesn't make sense to
query the protocol number of a non-IP packet.

#### Building and Testing Your Controller

- Build and launch the controller:

  ```shell
  $ make Firewall.d.byte
  $ ./Firewall.d.byte
  ```

- In a separate terminal window, start Mininet using the same
  parameters you've used before:

  ```
  $ sudo ./mn --controller=remote --topo=single,3 --mac
  ```

- Test to ensure that pings fail within Mininet:

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

- Although ICMP is blocked, other traffic, such as Web traffic should
  be unaffected. To ensure that this is the case, try to run a Web server
  on one host and a client on another.


  * In Mininet, start new terminals for `h1` and `h2`:

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

    This command should succeed.

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
