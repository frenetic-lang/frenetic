load policy examples/optical-bridge/naive.kat [1:1;2:1;] [1:1;2:1;]
load fabric examples/optical-bridge/fabric.kat [6:3;6:4;4:2;5:2;] [6:3;6:4;4:2;4:3;]
load topology examples/optical-bridge/topo.kat
compile fabric
show fabric
compile edge
show table 1 edge
show table 2 edge
show table 3 edge
show table 4 fabric
show table 5 fabric
show table 6 fabric

