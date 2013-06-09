Chapter 7: Firewall Redux
=========================

In [Chapter 3](03-OxFirewall.md), you wrote a firewall that blocks ICMP traffic using OpenFlow and Ox. You did this in two steps: first, you wrote a _packet_in_ function and then configured flow table to implement the same function efficiently. 
This NetCore program has the same features: `if frameType = 0x800 && and ipProtocol = 1 then drop else all`. 

In this chapter, you'll implement a more interesting firewall policy, for a trivial, one-switch topology. But, in the next chapter, you'll see that the policy you write in this chapter is easy to reuse and apply to any other topology.

## Topology 

In this chapter, you'll work the following network of four hosts and one switch:

![image](images/topo-single-4.png)

The host with MAC address `00:00:00:00:0n` is connected to port `n`. Mininet has builtin support for building single-switch topologies:

```
$ sudo mn --controller=remote --topo=single,4 --mac
```

### Exercise 1: Routing

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
    
monitorTable(1, routing)
```

Fill in the rest of the policy by editing [Chapter7.nc](netcore-tutorial-code/Chapter7.nc). 

#### Testing

Launch Frenetic in one terminal:

```
$ frenetic Chapter7.nc
```

And Mininet in another:

```
$ sudo mn --controller=remote --topo=single,4 --mac
```

Using Mininet, ensure that you can ping between all hosts:

```
mininet> pingall
```

## Access Control Policy

Now that basic connectivity works, you should enforce the access control policy written in the table below:

<table>
<tr>
  <th style="visibility: hidden"></th>
  <th style="visibility: hidden"></th>
  <th colspan="4">Server MAC address</th>
</tr>
<tr>
  <th style="visibility: hidden"></th>
  <th style="visibility: hidden"></th>
  <th>00:00:00:00:00:01</th>
  <th>00:00:00:00:00:02</th>
  <th>00:00:00:00:00:03</th>
  <th>00:00:00:00:00:04</th>
</tr>
<tr>
  <th rowspan="5" style="-webkit-transform:rotate(270deg)" >
    Client MAC<br>address
  </th>
  <th>00:00:00:00:00:01</th>
  <td>HTTP, SMTP</td>
  <td>HTTP, SMTP</td>
  <td>Deny All</td>
  <td style="background-color: lightblue">HTTP</td>
</tr>
<tr>
  <th>00:00:00:00:00:02</th>
  <td>HTTP, SMTP</td>
  <td>HTTP, SMTP</td>
  <td>Deny All</td>
  <td>HTTP</td>
</tr>
<tr>
  <th>00:00:00:00:00:03</th>
  <td>HTTP, SMTP</td>
  <td>SMTP</td>
  <td>HTTP, SMTP</td>
  <td>HTTP, SMTP</td>
</tr>
<tr>
  <th>00:00:00:00:00:04</th>
  <td>HTTP, SMTP</td>
  <td>SMTP</td>
  <td>HTTP, SMTP</td>
  <td>HTTP, SMTP</td>
</tr>
</table>

Each cell in this table has a list of allowed protocols for connections between
clients (rows) and servers (columns)





this this, each cell specifies the 

In this table, the values in each cell state the allowed allowed TCP destination ports for connections from the source to the destination.

> Recall that HTTP is port 80, SSH is port 22, and SMTP is port 25.

For example, the cell in to top-right corner states that host 1 is only allowed to send packets to port 80 of host 4. Any flow not explicitly mentioned is disallowed.

### Exercise 2

Wrap the routing policy you wrote above within a fire-walling policy.

> See [Sol_Chapter7_1.nc](netcore-example-code/Sol_Chapter7_1.nc), if you did not
> finish the previous task.

Your edited file will probably have the following structure:

```
let routing = (* from part 1, above *)

let firewall =
  if (* traffic-allowed *) then
    routing
  else
    drop

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

To facilitate testing, you can simply run three copies of _fortune_ on each host that listen on the HTTP, SSH, and SMTP ports. We've included a shell script, `netcore-tutorial-code/fortune-http-ssh-smtp.sh` that does just that. Run it on each host:

```
mininet> h1 ./fortunes.sh &
mininet> h2 ./fortunes.sh &
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
