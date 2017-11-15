(** Module for generating ProbNetKAT encoding of topology with certain failure
    model.
*)
open Syntax
module Net = Frenetic.Network.Net

let parse : string -> Net.Topology.t =
  Net.Parse.from_dotfile

let to_probnetkat (topo : Net.Topology.t) : string policy =
  let open Net.Topology in
  fold_edges (fun edge t ->
    let (src_sw,src_pt) = edge_src edge in
    let (dst_sw,dst_pt) = edge_dst edge in
    t  
    )
    topo
    PNK.drop
