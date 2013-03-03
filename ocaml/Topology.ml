type link = Link of MessagesDef.switchId * Packet.portId

type topology = Topology of (link * link) list

let reverse_edge (sp1, sp2) = 
  (sp2, sp1)

let bidirectionalize topo = 
  List.fold_left (fun acc edge -> edge::(reverse_edge edge)::acc) [] topo
