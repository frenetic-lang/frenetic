load policy examples/optical-fork/ideal-1.kat [1:1;2:1;3:1;] [1:1;2:1;3:1;]
load circuit examples/optical-fork/circuit.kat [4:2;4:3;5:2;6:2;] [4:2;4:3;5:2;6:2;]
load topology examples/optical-fork/topo.kat
compile circuit
compile edge
show table 1 edge
show table 2 edge
show table 3 edge
show table 4 fabric
show table 5 fabric
show table 6 fabric
show table 7 fabric
