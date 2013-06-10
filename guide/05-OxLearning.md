Chapter 5: Learning Switch
==========================

In this exercise, you will build an Ethernet learning switch that learns the
location of each host.  As with previous exercises, you will begin by writing
and testing a learning function and then implement it efficiently using flow
tables.

### The Learning Switch Function

Thus far, you have provided connectivity simply by forwarding each packet out
every port, aside from the one on which it arrived. Early network devices
(called 'hubs') used this strategy, but it was quickly discovered that flooding
each packet throughout the network does not scale as the network grows.  As an
alternative, we can begin by flooding packets through the network but
eventually learn the location of each host.  Once we learn the location of two
hosts, we can forward traffic between them directly.  Indeed, such a device is
called a learning switch, and it has come to replace hubs in modern networks.

A learning switch has two logically distinct components:

- The *learning module* builds a table that maps hosts (mac addresses)
  to the switch port on which they are connected. The learning module
  builds this table by inspecting the source ethernet address and inport
  of every packet at the switch.

- The *routing module* uses the table generated above to route traffic
  directly to its route. That is, if the switch receives a packet for
  destination _X_ and the learning module has learned that _X_
  is accessible through port _N_, then the routing module forwards the
  packet directly out port _N_. (If the table does not have an
  entry for _X_, it simply floods the packet.)

Naturally, you will begin by writing a `packet_in` function that learns host
locations. 

#### Programming Task

Use [Learning.ml](ox-tutorial-code/Learning.ml) as a template.  It contains a
hash table to map hosts to ports:

```ocaml
let known_hosts : (dlAddr, portId) Hashtbl.t = Hashtbl.create 50 (* initial capacity *)
```

You can use `Hashtbl.add` to add a new host/port mapping:

```ocaml
Hashtbl.add known_hosts <pkt_src> <pkt_in_port>
```

* Modify the `learning_packet_in` function in [Learning.ml](ox-tutorial-code/Learning.ml)
to extract the ethernet source address and input port from incoming packets; and
* fix `packet_in` to extract the ethernet destination address; and
* update `packet_in` to invoke `learning_packet_in` and then `routing_packet_in`.

#### Compiling and Testing your Learning Switch

You should first test that your learning switch preserves connectivity by
sending ICMP messages between each host pair.  Next, use `tcpdump` to ensure
that your learning switch stops flooding once it learns the locations of two
hosts.

- Build and launch the controller:

  ```shell
  $ make Learning.d.byte
  $ ./Learning.d.byte
  ```

- In a separate terminal window, start Mininet:

  ```shell
  $ sudo mn --controller=remote --topo=single,3 --mac
  ```

- Test all-pairs connectivity:

  ```
  mininet> pingall
  ```

- Run `pingall` again to ensure that connectivity remains after the first round
of learning:

  ```
  mininet> pingall
  ```

At this point, your learning switch should have learned the locations of all
three hosts.  To test that your controller no longer floods traffic, we will
invoke `tcpdump` to monitor packets arriving at `h1` while sending traffic
from `h2` to `h3`.  No traffic should reach `h1`:

  * In Mininet, start new terminals for `h1`, `h2`, and `h3`:

    ```
    mininet> xterm h1 h2 h3
    ```

  * In the terminal for `h1`, start `tcpdump`:

    ```
    # tcpdump -c 1 port 80
    tcpdump: verbose output suppressed, use -v or -vv for full protocol decode
    listening on h1-eth0, link-type EN10MB (Ethernet), capture size 65535 bytes
    ```

  * In the terminal for `h1`, start a local fortune server:
  
    ```
    # while true; do fortune | nc -l 80; done
    ```

  * In the terminal for `h3`, fetch a fortune from `h2`:

    ```
    # curl 10.0.0.2:80
    ```

  * Finally, check the status of `tcpdump` in the terminal for `h1`; it should
    still be hanging, listening for an incoming packet.  If it terminated with
    the message `1 packet captured`, then your controller sent a packet to `h1`
    as well as `h2`.

    > Note that this will fail if you have not already used `ping` to learn the
    > locations of `h2` and `h3`.

### An Efficient Learning Switch

Sending traffic directly to its destination is a clear improvement over simply
flooding, but we can do better. Just as in each previous chapter, we would like
to install rules to keep forwarding on the switch. But this time we cannot be
entirely proactive.  Imagine, for a moment, that we intend our controller to
work seamlessly with any number of hosts with arbitrary MAC addresses, rather
than the prescribed topology we have tested with so far.  In this case, we must
ensure that the first packet sent from each host is directed to the controller,
so that we might learn its location.

#### Programming Task

Augment the `try..with` block in your `routing_packet_in` function to install
two new flows when a packet arrives and `known_hosts` contains the location of
the destination host.  Together, these two flows should enable direct,
bidirectional communication between the source and desination hosts.

> Hint: only install new flow rules when both the source and destination host
> locations are known.  Otherwise, the controller may not learn the location of
> every host. For example, when `h1` sends a packet to `h2`, we
> learn the location of `h1`. If we immediately install a rule directing all
> traffic destined for `h1` out the correct port, then it will match all future
> traffic from `h2` to `h1`, keeping traffic from `h2` in the dataplane (and
> hence never arriving at the controller and triggering `packet_in`).

After the controller learns the location of every host, no more packets should
arrive on the controller.

## Next chapter: [NetCore Introduction][Ch6]


[Ch6]: 06-NetCoreIntroduction.md

[Action]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.Action.html

[PacketIn]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.PacketIn.html

[PacketOut]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.PacketOut.html

[OxPlatform]: http://frenetic-lang.github.io/frenetic/docs/Ox_Controller.OxPlatform.html

[Match]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.Match.html

[Packet]: http://frenetic-lang.github.io/frenetic/docs/Packet.html
