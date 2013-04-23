open Platform0x04
open NetCoreFT

module G = Graph.Graph

let s1 = Int64.of_int 1
let s2 = Int64.of_int 2
let s3 = Int64.of_int 3
let s4 = Int64.of_int 4

let h1 = 1
let h2 = 2

let make_topo () = 
  let topo = G.create () in
  let () = G.add_switch topo s1;
    G.add_switch topo s2;
    G.add_switch topo s3;
    G.add_switch topo s4;

    G.add_host_edge topo h1 s1 (Int32.of_int 1);
    G.add_host_edge topo h2 s4 (Int32.of_int 1);

    G.add_edge topo s1 (Int32.of_int 2) s2 (Int32.of_int 1);
    G.add_edge topo s2 (Int32.of_int 1) s1 (Int32.of_int 2);

    G.add_edge topo s1 (Int32.of_int 3) s3 (Int32.of_int 1);
    G.add_edge topo s3 (Int32.of_int 1) s1 (Int32.of_int 3);

    G.add_edge topo s2 (Int32.of_int 2) s3 (Int32.of_int 2);
    G.add_edge topo s3 (Int32.of_int 2) s2 (Int32.of_int 2);
    
    G.add_edge topo s2 (Int32.of_int 3) s4 (Int32.of_int 2);
    G.add_edge topo s4 (Int32.of_int 2) s2 (Int32.of_int 3);

    G.add_edge topo s3 (Int32.of_int 3) s4 (Int32.of_int 3);
    G.add_edge topo s4 (Int32.of_int 3) s3 (Int32.of_int 3);
  in
  topo
