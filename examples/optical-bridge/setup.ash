load policy examples/optical-bridge/naive.kat [1:1;2:1;3:3;] [1:1;2:1;3:3;]
load fabric examples/optical-bridge/fabric.kat [6:3;6:4;4:2;5:2;] [6:3;6:4;4:2;5:2;]
load topology examples/optical-bridge/topo.kat
compile fabric
compile edge
install fabric [4;5;6;]
install edge
show table 1 edge
show table 2 edge
show table 3 edge

