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
    if dstMAC = 00:00:00:00:00:01 then
      fwd(1)
    else if dstMAC = 00:00:00:00:00:02 then
      fwd(2)
    else
      fwd(3)
  else if switch = 2 then
    (* fill in policy for 2 *)
  else if switch = 3 then
    (* fill in policy for 3 *)
  else
    drop
```

Unfortunately, simply connecting hosts isn't suff

      


