Chapter 8:  Dynamic NetCore
===========================

So far in this tutorial, we have used NetCore to write static network
policies --- policies that do not change in response to network traffic or
topology events such as a switch coming up or going down.  In general,
crafting a dynamic policy amounts to writing a program that generates a
*stream* of static policies.  For instance, a new static policy can be generated
each time a new switch comes on line or the load in the network reaches
some threshold or a new connection is initiated.  

NetCoreDSL makes it possible to experiment with simple dynamic
policies by providing a small number of dynamic building blocks
including a learning switch and a NAT box.  Intuitively, to create
a dynamic policy, one writes an ordinary static policy that 
includes a reference to a dynamic building block:

```
let policy = ... static_component ... dynamic_component ...
```
If the <code>dynamic_component</code> generates the following series of 
policies as it executes:
```
dynamic1

dynamic2

dynamic3 

...
```
then <code>policy</code> will be:
```
... static_component ... dynamic1 ...

... static_component ... dynamic2 ...

... static_component ... dynamic3 ...

...
```
In other words, the static components remain fixed as the dynamic subcomponent
fluctuates from one variant to the next.


### Example 5: NAT

In this example, we will investigate how to use a dynamic NAT
component to support connections initiated from a private machine "on
the inside," behind a public IP, to a remote machine "on the outside."
More specifically, when a machine on the inside initiates a connection
to a machine on the outside, NAT will pick a new, available public
port, and rewrite the source IP and port to use the available port and
the public IP. Responses to previously established port are rewritten
to use the private source port and IP.

To use the built-in <code>nat</code> box, we supply it with a desired public IP
address (say, <code>10.0.0.254</code>) and invoke it as follows.
```
let translatePrivate, translatePublic = nat (publicIP = 10.0.0.254)
in ...
```
This expression generates a pair of dynamic 
components:  (1) <code>translatePrivate</code>
rewrites requests travelling from inside to 
outside, and (2) <code>translatePublic</code> rewrites requests travelling
from outside back in.  These two components can be used within the
policy following the keyword <code>in</code>.  The following
code defines the complete program. 
```
let natter =
  let translatePrivate, translatePublic = 
    nat (publicIP = 10.0.0.254) 
  in
    if switch = 1 && inPort = 1 then 
      (translatePrivate; if inPort = 1 then fwd(2) else pass)
  + if switch = 1 && inPort = 2 then
      (translatePublic; if inPort = 2 then fwd(1) else pass)

let app =
  if frameType = arp then all
  else monitorTable(1, natter)  
```

The policy can be found in <code>tut5.nc</code>.  Launch a controller
now:
```
$ frenetic tut5.nc
```
and start mininet as follows.
```
$ sudo mn  --controller=remote --mac
```
Next you will need to prime your arp cache on host <code>h2</code>
and start a simple web server running.
```
mininet> h2 arp -s 10.0.0.254 00:00:00:00:00:01
mininet> h2 python -m SimpleHTTPServer 80 . &
```
Finally, try fetching a web page from the server:
```
h1 wget -O - 10.0.0.2
```
When you do so, you should see the <code>tut5.nc</code> controller
print out an updated flow table for switch 1.  The flow table is 
printed as a list of rules, with the highest priority rule at the
top and the lowest priority rule at the bottom.  Each rule has
the form
```
{Packet Pattern} => [Actions]
```
The <code>Packet Pattern</code> is a list of conditions the packet
fields must satisfy to match the rule.  The <code>Actions</code>
are taken if the packet matches.  

After issuing the <code>wget</code> command,
you should see 2 rules matching packets coming <code>inPort</code> 1
and 2 rules matching packets coming <code>inPort</code> 2.
Take a look:  Which private port did this connection use?  Which
public port?

### Exercises

*This section very much TODO.*

1. When running tut5.nc, what happens if you try to ping h2 from h1?
```
mininet> h1 ping -c 1 h2
```
Modify the policy in some way to allow ping through.

2. Experiment with the Mac Learning Component.  A simple policy that
uses Mac Learning may be found in tut6.nc.
Consider running the policy on our tree-shaped topology.
*TODO: better picture*
```
        (s1)
        1  2
       /    \
      3      3
  (s2)       (s3)
  1  2       1  2
 /    \     /    \
h1     h2  h3    h4
```
This time start up mininet with random mac addresses (omit the
<code>--mac</cod> option.  
```
$ sudo mn --controller=remote --topo=tree,depth=2,fanout=2
```
Figure out what the mac addresses of each of the hosts are using
two different monitoring techniques.

Solutions to these exercises may be found in
guide/examples directory in files <code>sol2-1.nc</code> and
<code>sol2-2.nc</code>.

Summary
-------

NetCore rocks!  QED.

[topo_1]: images/topo_1.png "Default Mininet topology."
[topo_2]: images/topo_2.png "Simple linear topology."
[topo_3]: images/topo_3.png "Simple tree topology."
