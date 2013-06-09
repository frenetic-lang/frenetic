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
controller, for inspection, and wherever they may be destined in the network.

To support this idiom, you need new kind of operator on policies:
*parallel composition*, written `P1 + P2`. Intuitively, when applied to
a packet `pk`, the parallel composition creates two copies of `pk` and
applies `P1` to the first and `P2` to the second. 

Overall, it generates the *union* of the results from
<code>P1</code> and <code>P2</code>.  Hence, if <code>P1</code> forwards to A and <code>P1</code> forwards to B then <code>P1 + P2</code> makes a copy of the input packet and forwards to both A and B.

> TODO(arjun): continue below. Do an exercise on monitoring HTTP + routing





With this in mind, let's modify the port mapper to inspect the packets both
before and after the rewriting.  We can leave the mapper component unchanged
and add monitoring to the forwarder.

```
...
let before = if inPort = 1 then monitorPackets("BEFORE")
let after = if inPort = 1 then monitorPackets("AFTER")

let forwarder =
  (before + mapper); (all + after)
```

Above, we used a 
<code>if</code>-<code>then</code> statement (no else) to limit the packets
that reach the monitoring policy to only those packets satisfying
the <code>inPort = 1</code> predicate.  Otherwise, the monitor policy
prints *all* packets that reach it.  Note that if there is no <code>else</code>
branch in a conditional, packets not matching the conditional are dropped
(i.e., in this second case, the conditional produces the empty set of result
packets).

This new policy can be found in <code>Port_Map_Monitor1.nc</code>.
Test it as above using iperf, but this time watch the output in the 
controller window.  You should see lines similar to the following being printed:
```
[BEFORE] packet dlSrc=4a:f7:98:81:78:0d,dlDst=d6:7c:1e:d6:e3:0b,nwSrc=10.0.0.1,nwDst=10.0.0.2,tpSrc=52923;tpDst=5022 on switch 1 port 1
[AFTER] packet dlSrc=4a:f7:98:81:78:0d,dlDst=d6:7c:1e:d6:e3:0b,nwSrc=10.0.0.1,nwDst=10.0.0.2,tpSrc=52923;tpDst=22 on switch 1 port 1
```
You will notice <code>tpDst=5022</code> in lines marked
<code>BEFORE</code> and <code>tpDst=22</code> in lines marked 
<code>AFTER</code>.

Monitoring Load
---------------

Another useful query measures the load at different places in the network.  The
<code>monitorLoad(n, label)</code> policy prints the number of packets and
bytes it receives every <code>n</code> seconds.  Each output line from this
query is prefixed by the
string <code>label</code>, and we can restrict the packets monitored by
<code>monitorLoad</code> using <code>if</code>-<code>then</code> clauses.
Note that the implementation 
of <code>monitorLoad</code> is far more efficient 
than <code>monitorPackets</code> as the former does not send packets 
to the controller.

<code>Port_Map_Monitor2.nc</code> contains a variation of the port mapper 
that monitors load instead of packets.  You can test it by
issuing a longer iperf request 
(adjust the timing parameter <code>-t seconds</code>).  Watch the load 
printed in the controller terminal.
The following command runs iperf for <code>20</code> seconds.
```
mininet> h1 iperf -c 10.0.0.2 -p 5022 -t 20
