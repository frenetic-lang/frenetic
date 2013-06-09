 # Chapter 7: Firewall Redux

In [Chapter 3](03-OxFirewall.md), you used OpenFlow and the Ox
controller to write a firewall that blocks ICMP traffic. You first
wrote the `packet_in` function, and then configured the switch's flow
table to implement the same function efficiently.

This NetCore program does all of the above:

```
if dlTyp = 0x800 && and nwProto = 1 then drop else all
```

In this chapter, you'll implement a more interesting firewall policy
with NetCore. You'll work with a network of four hosts and one switch, depicted below:

![image](images/topo-single-4.png)

The host with MAC address `00:00:00:00:0n` is connected to port `n`.

### Programming Task 1

Write a routing policy for this network. Use `monitorTable` to examine the flow table that the compiler generates and try a few `ping`s between hosts.


As you've seen, NetCore supports ordinary `if`-`then`-`else` expressions.
So, you can implement the routing policy as follows:

```
let routing =
  if dstMAC=00:00:00:00:00:01 then
     fwd(1)
  else if (* destination is 2, forward out port 2, etc. *)
    ...
  else if dstMAC=ff:ff:ff:ff:ff:ff then
    all (*  allow broadcasts *)
  else
    drop
    
` r Nlet main =
  monitorTable(1, routing)
```

Save the policy to a file called `Routing.nc` and launch Frenetic in one terminal:

```
$ frenetic Routing.nc
```

And Mininet in the other:

```
$ sudo mn --controller=remote --topo=single,4 --mac
```

In the controller's terminal, you should see the simple, synthesized flow table,
and in the Mininet terminal, you should be able to ping between all hosts:

```
mininet> pingall
```

### The Firewall Policy

Now that basic connectivity works, your task is to implement the following firewall policy:

<table>
<tr>
  <th style="visibility: hidden"></th>
  <th style="visibility: hidden"></th>
  <th colspan="4">Destination MAC address</th>
</tr>
<tr>
  <th style="visibility: hidden"></th>
  <th style="visibility: hidden"></th>
  <th>00:..:01</th>
  <th>00:..:02</th>
  <th>00:..:03</th>
  <th>00:..:04</th>
</tr>
<tr>
  <th rowspan="5" style="-webkit-transform:rotate(270deg)" >
    Source MAC<br>address
  </th>
  <th>00:..:01</th>
  <td>SSH, HTTP, SMTP</td>
  <td>SSH, HTTP, SMTP</td>
  <td>Deny All</td>
  <td>HTTP</td>
</tr>
<tr>
  <th>00:..:02</th>
  <td>SSH, HTTP, SMTP</td>
  <td>SSH, HTTP, SMTP</td>
  <td>Deny All</td>
  <td>HTTP</td>
</tr>
<tr>
  <th>00:..:03</th>
  <td>SSH, HTTP, SMTP</td>
  <td>SMTP</td>
  <td>SSH, HTTP, SMTP</td>
  <td>SSH, HTTP, SMTP</td>
</tr>
<tr>
  <th>00:..:04</th>
  <td>SSH, HTTP, SMTP</td>
  <td>SMTP</td>
  <td>SSH, HTTP, SMTP</td>
  <td>SSH, HTTP, SMTP</td>
</tr>
</table>

### Programming Task 2

To help you get started, see the file [Firewall.nc](netcore-tutorial-code/Firewall.nc). You'll need to fill in the firewall policy and paste in your routing policy. The combined policy will have the form:

```
let routing = (* from part 1, above *)

let firewall =
  if (* traffic-allowed *) then
    routing
  else
    drop

let main =
  monitorTable(1, firewall)
```

#### Testing

Launch Frenetic in one terminal:

```
$ frenetic Firewall.nc
```

And Mininet in another:

```
$ sudo mn --controller=remote --topo=single,4 --mac
```

To facilitate testing, you can simply run fortune servers on all hosts, listening on the HTTP, SSH, and SMTP ports. We've included a shell script, `netcore-tutorial-code/fortune-http-ssh-smtp.sh` that does just that. Run it on each host:

```
mininet> h1 ./fortune-http-ssh-smtp.sh &
mininet> h2 ./fortune-http-ssh-smtp.sh &
mininet> h3 ./fortune-http-ssh-smtp.sh &
mininet> h4 ./fortune-http-ssh-smtp.sh &
```

To throughly test the policy, you need to open a connection between each pair
of hosts, over each port. There is no need for such a comprehensive test in this tutorial. But, you should try a few. For example, ensure that `h1` cannot connect to any port on `h3`:

```
mininet> h1 curl 10.0.0.3:80 # should fail
mininet> h1 curl 10.0.0.3:25 # should fail
mininet> h1 curl 10.0.0.3:22 # should fail
```

> TODO(arjun): fill in the actual error message above

> TODO(arjun): Consider providing a testing script if it provides any value.


## Next chapter: [NetCore Composition][Ch7]
