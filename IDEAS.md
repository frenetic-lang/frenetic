# IDEAS

## Bayesian Networks
Representing joint distributions naively does not scale in general. For example, the joint distribution of n Bernoulli variables is given by 2^n parameters. But if the variables are independent, n parameters suffice.
Can we use ideas from Bayesian networks to keep the representation of distributions factorized?
-> study the literature

## Maintain factorization when converting to linear system and back
Factorization is of little use if we have to "mutiply-out" and thus incur an exponential blowup when we iterate (i.e., use the linear algebra backend). Can we keep things factorized even when iterating? I.e., maybe there is a "factorized" linear system that is smaller than the naive linear system would be?

## Use conditioning instead of while loops
It can be implemented much more efficiently, thus speeding up resilient random walk, resilient ecmp, etc. It will also allow us to condition on "no all outgoing links fail" to verify resilience theorems.

## Local fields
Reason about fact that local fields go out of scope eventually. This can be exploited for optimization. How to implement such optimizations? Algebraic manipulations? See my notes.

## Quickcheck
There are various undocumented invariances and properties that hold of the implementation. Would be great to write these down as quickcheck properties.


* Instead of using one global domain, refine domain during matrix compilation?
* One bottleneck: matrix -> fdd
    - how can we speed this up?