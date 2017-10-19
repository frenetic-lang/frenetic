open Routing_Mw
open Routing_Frt
open Routing_Types

open Frenetic_Network
open Net

(* multiplicative weights input *)
module MWInput : MW_INPUT with type structure = FRT.routing_tree = struct

  type structure = FRT.routing_tree

  let select_structure (topo : topology) (_:demands) (nodes : Topology.VertexSet.t) =
    (* First, make an FRT tree decomposition from the topology. *)
    let tree = FRT.make_frt_tree topo in
    let node_list = Topology.VertexSet.elements nodes in
    (FRT.generate_rt topo tree node_list, 1.)

  let usage_of_structure (_ : topology) (_:demands) (st : FRT.routing_tree) =
    FRT.usage_of_tree st

  let set_weight topo edge w =
    let label = Topology.edge_to_label topo edge in
    Link.set_weight label w; topo

  let get_weight topo edge =
    let label = Topology.edge_to_label topo edge in
    Link.weight label

end

(* multiplicative weights instantiation *)
module RRTs : MW_ALG with type structure = FRT.routing_tree = Routing_Mw.Make (MWInput)
