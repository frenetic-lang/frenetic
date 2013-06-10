Chapter 9: Monitoring with NetCore
==================================

In [Chapter 4](04-OxMonitor.md), you wrote an Ox controller that measured the volume of HTTP traffic on the network. You first wrote the _packet_in_
function to count all HTTP traffic on the controller. To count packets efficiently, you had to:

- Create flow table rules that match HTTP traffic exclusively,
- Periodically send OpenFlow statistics requests to the switch,
- Interpret the OpenFlow statistics replies, and
- Calculate the sum of HTTP request and HTTP replies.

The following NetCore program does all of the above:

```
if tcpSrcPort = 80 || tcpDstPort = 80 then monitorLoad(5, "HTTP traffic")
```

However, unlike the Ox controller, which flooded all traffic, this NetCore program doesn't forward any traffic it all. Instead, you can compose it
with any routing policy. In this chapter you'll learn how to do so using
NetCore's _parallel composition_ operator.

## Parallel Composition

When you monitor the network, you do so *in parallel* with the routing policy. Intuitively, you want to send packets to two places: the
controller, for inspection, and wherever they may be destined in the network. To support this idiom, you need new kind of operator on policies:
*parallel composition*, written `P1 + P2`. Intuitively, when applied to
a packet `pk`, the parallel composition creates two copies of `pk` and
applies `P1` to the first and `P2` to the second.  Overall, it generates the *union* of the results from
<code>P1</code> and <code>P2</code>.  Hence, if <code>P1</code> forwards to A and <code>P1</code> forwards to B then <code>P1 + P2</code> makes a copy of the input packet and forwards to both A and B.

### Exercise: Modular Web Monitoring

Cloud service providers will typically bill their clients in accordance to the resources they use.  Your task in this exercise is to set up monitoring infrastructure for the network developed in Chapter 8.  To do so, switch to the Chapter 9 directory.
```
cd Chapter9
```
Proceed as follows:

- First, create a new file, `Monitor.nc`.  Inside this file, create infrastructure for monitoring the web traffic sent by `h2`. Create a different string label for each of the other hosts ("H2->H1", "H2->H3", "H2->H4") and use `monitorLoad` to record the HTTP packets send from `h2` to each of the other hosts separately.
- Second, modify the template `Main.nc` to compose your monitoring policy with the firewall and router defined in Chapter 8

When you are done, testing your monitor.

- Start mininet and launch an `xterm` on host `h2`:
```
$ sudo mn --controller=remote --topo=tree,2,2 --mac --arp
mininet> xterm h2
```
- Set up a fortune server on host `h2` :
```
$ while true; do fortune | nc -l 80; done 
```
- Fetch fortunes from the other hosts.  For example:
```
mininet> h1 curl 10.0.0.2:80
```

During your experiments, you should should see the load between `h1` and `h2` escalate.  Your firewall should block requests from `h3` and `h4` to `h2` so you should never see the load from `h2` to `h3` or `h4` escalate.

## Monitoring Packets

When debugging complex policies, it can be useful to peer in to the
middle of the network.  If you need this kind of "printf"-style debugging support when implementing a policy, you may use 
the <code>monitorPackets(label)</code> policy, which sends every input packet it
receives to the controller as opposed to forwarding it along a network data
path (like the <code>fwd(port)</code> policy does).  At the controller, the
packet is printed with the string <code>label</code> as a prefix and then
discarded.

## Under the Hood

Sequential and parallel composition make it easier to write SDN controller
programs, but it all gets compiled to OpenFlow rules in the end.  To get a feel
for how the compiler works, let's take another look at a NetCore version of the
efficient firewall from the [OxFirewall](03-OxFirewall.md) chapter, altering it
slightly to block SSH rather than ICMP traffic:

```
let firewall = if !(tcpDstPort = 22) then all in
monitorTable(1, firewall)
```

We added a table query to show the flow table that NetCore produces for the
firewall.  Now, fire up the firewall
([Ox_Firewall.nc](netcore-tutorial-code/Ox_Firewall.nc)).  You should see the
following output:

```
$ frenetic netcore-tutorial-code/Ox_Firewall.nc
Flow table at switch 1 is:
 {dlTyp = ip, nwProto = tcp, tpDst = 22} => []
 {*} => [Output AllPorts]
 {*} => [Output AllPorts]
```

As you can see from the latter two rules, NetCore is less efficient 
(in terms of switch rule space used) than a
human programmer.  (But not for long, we hope!)  Nevertheless, this should look
very similar to the flow table you programmed.

Now, let's add a query to monitor traffic for <code>h1</code>:
```
let firewall = if !(tcpDstPort = 22) then all in
let monitor = if srcIP = 10.0.0.1 then monitorLoad(10, "From H1") in
monitorTable(1, firewall + monitor)
```

The flow table should look something like this:
```
$ frenetic netcore-tutorial-code/Ox_Firewall_Monitor.nc
Flow table at switch 1 is:
 {dlTyp = ip, nwSrc = 10.0.0.1, nwProto = tcp, tpDst = 22} => []
 {dlTyp = ip, nwProto = tcp, tpDst = 22} => []
 {dlTyp = ip, nwSrc = 10.0.0.1} => [Output AllPorts]
 {*} => [Output AllPorts]
```

Let's break it down, rule by rule:
* **Rule 1**: drop SSH traffic from <code>h1</code>.
* **Rule 2**: drop SSH traffic.
* **Rule 3**: forward traffic from <code>h1</code>.
* **Rule 4**: forward all other traffic.

Why so many rules?  OpenFlow switches can only count packets as they match a
rule.  Rule 1, for example, is necessary to precisely count traffic from <code>h1</code>.
Without it, our query would miss any SSH traffic sent from <code>h1</code>, as it would be
lumped in with all the other SSH traffic dropped by rule 2.

In general, NetCore creates a flow table for two policies joined by parallel
composition (<code>P1 + P2</code>) by creating flow tables for <code>P1</code>
and <code>P2</code>, and taking the Cartesian product of these tables, and then
concatenating the original tables.  The result looks like this:

![Parallel composition.][parallel_composition]

Section **A** in the flow table is the Cartesian product.  Packets that match
both <code>P1</code> and <code>P2</code> are matched here.  Because **A** is
given a higher priority, any packets that reach **B** or **C** may match
<code>P1</code> *or* <code>P2</code>, but not both.

As an aside, NetCore policies are total functions: they always process every
packet, even if that "processing" is simply to drop it.  Hence, NetCore adds a
final, catch-all rule to the flow table to drop packets that are not matched
higher up.



## Next chapter: [Tutorial Wrap-up][Ch10]

[Ch10]: 10-WrapUp.md
