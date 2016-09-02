load policy examples/optical-fork/ideal-1.kat [1:1;2:1;3:1;] [1:1;2:1;3:1;]
load circuit examples/optical-fork/circuit.kat [4:2;4:3;5:2;6:2;] [4:2;4:3;5:2;6:2;]
load topology examples/optical-fork/topo.kat
compile circuit
synthesize
