Chapter 6: Introducing NetCore
==============================

You've learned how to write controllers with OpenFlow. We've shown
you a simple, two step recipe to implement policies:

* You can easily write a _packet-in_ function to have the controller
  implement any policy, though it may be a very inefficient
  implementation.

* You can then use flow tables and statistics to program switches to
  implement the same policy efficiently.

In the following chapters, we introduce a new way to program SDN policies.
You write policy-functions in a little language we call **NetCore**.
The NetCore compiler will then synthesize the flow tables needed to
implement your policy-function efficiently. (It also
sends statistics requests, accumulates replies, manages
switch connections, and more.)

The templates and solutions for this part of the tutorial are in
the `netcore-tutorial-code` directory:

```
$ cd guide/netcore-tutorial-code
```

### Example 1: A Naive Repeater (Redux)

In the [OxRepeater](02-OxRepeater.md) chapter, you learned how to program an
efficient repeater by adding rules to the switch flow table.  Recall that a
repeater simply forwards incoming packets out all other ports.

In this example, we will begin by considering a network with just one switch
with two ports, numbered 1 and 2:  

![Default Mininet topology.][topo_1]

Our first goal will be to program a
switch-specific repeater that forwards traffic arriving at port 1 out port 2,
and vice versa.  The following NetCore policy accomplishes that task.

```
(* a simple repeater *)

let repeater =
  if inPort = 1 then fwd(2)
  else fwd(1) in
monitorTable(1, repeater)
```

As in OCaml, NetCore comments are placed within <code>(*</code> and
<code>*)</code> (and comments may be nested). The <code>let</code> keyword
introduces a new policy, which we have chosen to call <code>repeater</code>.
An <code>if</code>-<code>then</code>-<code>else</code> statement determines
whether to forward a packet out port 1 or port 2, depending on the packet's
<code>inPort</code> field.  In addition to testing the packet's
<code>inPort</code>, if statement predicates can refer to the
<code>switch</code> at which a packet arrives, as well as any of the
OpenFlow-supported fields, such as the <code>srcIP</code>, <code>dstIP</code>
or <code>dlTyp</code>.  Conditions can also be formed using conjunctions
(<code>&&</code>), disjunctions (<code>||</code>) and negation (<code>!</code>)
of other conditions.  See the [manual](A-NCManual.md) for the complete list of
predicates. 

The last line of the program uses <code>monitorTable(1,repeater)</code>, which
will print the flow table generated for switch <code>1</code> from the
<code>repeater</code> policy.  It is equivalent to <code>repeater</code>, but
with the side effect of printing the flow table.  Now, when you run the
example, take a look at the flow table that the NetCore compiler creates for
you and compare it to your flow table rules from the Ox tutorial.

#### Run the Example

Within the <code>netcore-tutorial-code</code> directory, you should
find the repeater policy in <code>Repeater.nc</code>.  To start the
repeater controller, just type:
```
$ frenetic Repeater.nc
```
Now, in a separate terminal, start up mininet with the default, single
switch topology.
```
$ sudo mn --controller=remote
```

#### Test the Example

At the mininet prompt, test your repeater program by pinging <code>h2</code> from <code>h1</code>:
```
mininet> h1 ping -c 1 h2
```
You should see a trace like this one:
```
PING 10.0.0.2 (10.0.0.2) 56(84) bytes of data.
64 bytes from 10.0.0.2: icmp_req=1 ttl=64 time=0.216 ms

--- 10.0.0.2 ping statistics ---
1 packets transmitted, 1 received, 0% packet loss, time 0ms
rtt min/avg/max/mdev = 0.216/0.216/0.216/0.000 ms
```
Ping <code>h1</code> from <code>h2</code> as well.
Once you are convinced the repeater works,
try replacing the given repeater with an even simpler one:
```
let repeater = all in
monitorTable(1, repeater)
```
The <code>all</code> policy forwards any packet arriving at a switch out
all ports on that switch except the port it arrived on.  Try testing
that out too to see if you have done it correctly.

The opposite of the <code>all</code> policy is the <code>drop</code> policy,
which drops all packets on the floor.  

## Next chapter: [Firewall Redux][Ch7]

[Ch7]: 07-NCFirewall.md

[topo_1]: images/topo_1.png "Default Mininet topology."
[topo_2]: images/topo_2.png "Simple linear topology."
[topo_3]: images/topo_3.png "Simple tree topology."
