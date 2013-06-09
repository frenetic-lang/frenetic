Chapter 9: Monitoring with NetCore
==================================

```

                                     *** TOP SECRET ***
                          
Dear Network Administrator,

Due to the increased use of formal methods in networking, it is becoming very 
difficult for us to monitor your email by old-fashioned hacking. Please comply
with this formal order to give us indirect access to your SMTP traffic.

                              Cheers,
                              
                              NSA
```

In this chapter, you will comply with the NSA's request for access to your SMTP traffic. You will also learn about _parallel composition_ in NetCore.

When one monitors a network, one does so *in parallel* with some
standard forwarding policy; one would like the packets to go *two* places:  to
the spies for inspection, and to their legitimate destination on the network.

To support this idiom, we must introduce a new kind of operator on policies:
*parallel composition*.  Intuitively, when supplied with a packet
<code>p</code> as input, the parallel composition <code>P1 + P2</code> applies
<code>P1</code> to <code>p</code> and also, independently,
applies <code>P2</code> to a second copy of
<code>p</code>.  Overall, it generates the *union* of the results from
<code>P1</code> and <code>P2</code>.  Hence, if <code>P1</code> forwards to A
and <code>P1</code> forwards to B then <code>P1 + P2</code> makes a copy of the
input packet and forwards to both A and B.
