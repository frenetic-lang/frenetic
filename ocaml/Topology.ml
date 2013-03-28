type link = Link of OpenFlow0x01Types.switchId * Packet.portId

type topology = Topology of (link * link) list

let reverse_edge (sp1, sp2) = 
  (sp2, sp1)

let bidirectionalize (Topology topo) = 
  let topo' = 
    List.fold_left 
      (fun acc edge -> edge::(reverse_edge edge)::acc) 
      [] topo in 
  Topology topo'
