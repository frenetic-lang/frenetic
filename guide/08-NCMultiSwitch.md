# Chapter 8: Multiple Switches (at last!)

In this chapter, you'll finally work with a multi-switch network. First, you'll write and test a routing policy. Then, you'll use the firewall policy you wrote in the previous chapter and apply it to the new network. In fact, you'll learn how package your firewall into a reusable module that you can compose with any other policy. You'll accomplish this using a key feature of NetCore: _sequential composition_.

## Topology


You will work with the following tree topology:

![image](images/topo-tree-2-2.png)

The figure labels hosts as before, and also labels switches and their ports. You can create this topology easily with Mininet:

```
$ sudo mn --controller=remote --topo=tree,2,2 --mac
```
> `tree,2,2` creates a topology of height 2 and fanout 2


### Exercise 1: Routing

Using NetCore, write a routing policy that connects all hosts to each other. You already know how to do this for a single switch. To write a multi-switch routing policy, you can use the `switch = n` predicate as follows:

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

You'll find this template in [Chapter8.nc](netcore-tutorial-code/Chapter8.nc). Fill it in.

#### Testing

Launch Frenetic in one terminal:

```
$ frenetic Routing2.nc
```

And Mininet in another, then run `pingall`:

```
$ sudo mn --controller=remote --topo=tree,2,2 --mac
mininet> pingall
```

## A Reusable Firewall Using Sequential Composition

Now that basic connectivity works, your goal is to apply exactly the same access control policy you built in the
last chapter to this new network. Unfortunately, you cannot simply reuse the firewall in its current form, since it has baked-in the routing policy for the one-switch network. i.e., the policy from [Chapter 7][Ch7] looks like
this:

```
let routing = (* routing for 1 switch only *)

let firewall =
  if (* traffic allowed *) then
    routing
  else
    drop
```

We need a way to truly separate the routing policy and the firewall policy. To do so, we will use a NetCore operator called _sequential composition_. Sequential composition lets you take any two policies, `P` and `Q`,
and run them in sequence:

```
P; Q
```

This form of composition is akin to pipes in Unix. You can think of `P;Q` as a way to pipe the packets produced by `P` into the policy `Q`. To achieve complex tasks, you can string a long chain of policies together, `P1;P2;P3; ...` just as you can use pipes to compose several different Unix programs together.

You've probably used `grep` and pipes on a Linux to filter lines of text. You can similarly use sequential composition to filter (i.e., firewall) packets:

`firewall; routing`

For this to work, you do need to make one small change to `firewall`: you will replace all occurrences of`routing` within the `firewall` to use the special action `pass`, which is the identity function on packets.
By having the firewall just `pass` or `drop`, you can compose it with any routing policy.

### Exercise 2: Abstracting the Firewall

In this exercise, you'll move the firewall you wrote in the last chapter to its own file, `Firewall.nc`, edit it to just `pass` and `drop` packets, and then include `Firewall.nc` into both `Chapter7.nc` and `Chapter8.nc`.

- Move the code for `firewall` function from `Chapter6.nc` into a new file called `Firewall.nc`.

- In `firewall`, you have (possibly several) occurrences of `routing` (i.e., the routing policy from Chapter 7).  
  Replace all occurrences of `routing` with `pass`, making the firewall agnostic to routing.

- Modify `Chapter7.nc`, changing the main expression to sequence `firewall` and `routing`:

  ```
  (* This is a sketch of Chapter7.nc *)
  include "Firewall.nc"
  
  let routing = ...
  
  firewall; routing
  ```
  
- Modify `Chapter8.nc` in exactly the same way: include `Firewall.nc` and then change the main expression to  
  `firewall; routing`.
  
#### Testing

You should test this firewall in exactly the same way you tested the firewall in [Chapter 6][Ch6].