# Chapter 8: Multiple Switches (at last!)

In this chapter, you'll finally work with a multi-switch network. First, you'll use write and test a routing policy for this network. Then, you'll use the firewall policy you wrote in the previous chapter and apply it to the new network. In fact, you'll learn how package your firewall into a reusable module that you can compose with any other policy. You'll accomplish this by using a key feature of NetCore: _sequential composition_.

You will work with the following tree topology:

![image](images/topo-tree-2-2.png)

The figure labels hosts as before, and also labels switches (with unique IDs) and switches' ports.

### Programming Task 1

Using NetCore, write a routing policy that connects all hosts to each other. You already know how to do this for a single
switch. To write a multi-switch routing policy, you can use the `switch = n` predicate as follows:

```
let routing =
  if switch = 1 then
    (* Policy for Switch 1 *)
    ...
  else if switch = 2 then
    (* Policy for Switch 2 *)
    ...
  else if switch = 3 then
    (* Policy for Switch 3 *)
    ...
  else
    drop
    
routing
```

You'll find this template in [Routing2.nc](netcore-tutorial-code/Routing2.nc). Fill it in.

#### Testing

Launch Frenetic in one terminal:

```
$ frenetic Routing2.nc
```

And Mininet in another:

```
$ sudo mn --controller=remote --topo=tree,2,2 --mac
```
> `tree,2,2` creates a topology of height 2 and fanout 2

You should be able to ping between all hosts in Mininet:

```
mininet> pingall
```

## The Firewall Policy

Now that basic connectivity works, let's apply an access control policy: exactly the same policy you used on the one-switch network in the last chapter.

Although your task is to apply the same policy, you can't reuse the code, since
it bakes in the routing policy for the one-switch network. So, instead of copying the firewall predicate, we'll turn it into a reusable firewall that can be composed
with either routing policy.

### Building a Reusable Firewall

- Two NetCore policies, `P` and `Q` can be strung together, by writing `P; Q`.
- This operation is called _sequential composition_
- Akin to Unix pipes
- You can think of policies as packet-processing functions, and `P; Q` simply
  pipes the packets produced by `P` into the policy `Q`

- You've probably used `grep` and pipes on a Linux to filter text.
- Similarly, you can use sequential composition to filter packets
  You're going to write `firewall; routing`
  
- Right now, `Firewall.nc` either drops packets that should be filterd,
  or routes packets itself. We need to have it etierh drop packets or
   _leave packets untouched_.
   
   
```
include "Firewall.nc"

let routing = (* your routing policy from Programming Task 1 *)

firewall; routing
```

#### Testing

- as in the previous chapter


M

  