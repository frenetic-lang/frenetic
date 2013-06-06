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

#### Building and Testing Your Firewall

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

In this part, you will extend your implementation of the firewall
function to also implement the firewall using flow tables.

> Use [Sol_Firewall1.ml] if necessary.

#### Programming Task

Fill in the `switch_connected` event handler. You need to install two
entries into the flow table--one for ICMP traffic and the other for
all other traffic. You can use the following template:

```ocaml
let switch_connected (sw : switchId) : unit =
  Printf.printf "Switch %Ld connected.\n%!" sw;
  send_flow_mod sw 0l (add_flow priority1 pattern1 actions1);
  send_flow_mod sw 0l (add_flow priority2 pattern2 actions2)
```

You have to determine the priorities, patterns, and actions in the
handler above. You might want to revisit the description of flow
tables in [Chapter 1]. Here is a quick refresher:


- *Priorities*: higher numbers mean higher priority

- *Action lists*: To drop traffic, you provide an empty list (`[]` in
  OCaml) of actions

- *Patterns*: In the previous chapter, you used the builtin pattern
  `match_all`, which you may use again if needed. You will certainly
  need to write a pattern to match ICMP packets. The [Ox Manual] has
  several example patterns to get you started. You'll need to know
  that the _frame type for IP packets is 0x800_ and the _protocol
  number for ICMP is 1_.

#### Building and Testing

Build and test the efficient firewall in exactly the same way you
tested the firewall function. In addition, you shouldn't observe
packets at the controller.

[Action]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.Action.html

[PacketIn]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.PacketIn.html

[PacketOut]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.PacketOut.html

[OxPlatform]: http://frenetic-lang.github.io/frenetic/docs/Ox_Controller.OxPlatform.html

[Match]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.Match.html

[Packet]: http://frenetic-lang.github.io/frenetic/docs/Packet.html
