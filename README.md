# Probabilistic NetKAT
This branch provides an implementation of (history-free) Probabilistic NetKAT based on Markov chains and forwarding decision diagrams (FDDs).

## Environment variables
* **J**: number of processes on local machine (default: # of logical cores)
* **RJ**: number of processes per remote machine (default: J)
* **RN**: number of remote machines to use (default: 0)
For example,
```bash
$ RN=24 J=32 probnetkat.bench 16 true true
```
will run the F10 experiment using the entire cluster (24 machines) and 32 processes per machine.

## Quick start
Our code is tested under Ubuntu and MacOS and will likely not run under Windows.

0) You need opam (v>=2.0), the OCaml package manager.
1) You need OCaml 4.07: `opam update && opam switch create 4.07.1`.
2) After switching to OCaml 4.07, make sure your environment variables are set correctly: `eval $(opam config env)`.
3) Install various python and C dependencies by running the script `install-dependencies.osx.sh` on MacOS or `install-dependencies.ubuntu.sh` on Ubuntu.
4) Install dune, the OCaml build system: `opam install dune`.
5) Run `make`. If dune complains about missing dependencies, follow its instructions, then run `make` again.
6) Run `make install`.
7) You can make sure things work correctly by running some tests: `make test`.


## Resources
* For a brief, high-level overview, you may want to check out our PPS
[extended abstract](https://www.cs.cornell.edu/~smolka/papers/mc-abstract.pdf),
[poster](https://www.cs.cornell.edu/~smolka/talks/pps18-poster.pdf), and
[blog post](https://pps2018.soic.indiana.edu/2018/01/09/probabilistic-program-equivalence-for-netkat/).
* For more in-depth technical details, check out our [technical report](https://arxiv.org/abs/1707.02772).
* The implementation uses a symbolic representation of programs called FDDs. This data structure was introduced in
[our paper on efficiently compiling deterministic NetKAT](https://dl.acm.org/citation.cfm?id=2784761).
