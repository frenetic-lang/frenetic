load policy examples/speckat/ideal-1.kat [1:1;2:1;3:1;] [1:1;2:1;3:1;]
load circuit examples/speckat/circuit.kat [4:2;4:3;5:2;6:2;] [4:2;4:3;5:2;6:2;]
load topology examples/speckat/topo.kat
compile circuit
compile edge
install fabric [4;5;6;7;]
install edge
