open Core.Std
open Frenetic_Network
open Net
open Routing_Mw
open Routing_Frt
open Routing_Rrt
open Routing_Types

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

let add_or_increment_path (fd : flow_decomp) (p : path) (r : probability) : flow_decomp =
  let new_value = match PathMap.find fd p with
  | None -> r
  | Some prior_value -> prior_value +. r
    in
  PathMap.add ~key:p ~data:new_value fd

let solve_raecke (t:topology) (d:demands) : scheme =
   let epsilon = 0.1 in
    let end_points =
      VertexSet.filter (Topology.vertexes t)
      ~f:(fun v -> let label = Topology.vertex_to_label t v in
        Node.device label = Node.Host) in
    let _,mw_solution,_ = RRTs.hedge_iterations epsilon t d end_points in
    let paths src dst : probability PathMap.t = List.fold_left mw_solution
      ~init:PathMap.empty ~f:(fun acc (rt,p) ->
          let routing_path = FRT.get_path rt src dst in
          let physical_path = FRT.path_to_physical rt routing_path in
          let physical_path' = Routing_Frt.FRT.remove_cycles physical_path in
          add_or_increment_path acc physical_path' p) in
    Topology.VertexSet.fold end_points ~init:SrcDstMap.empty
      ~f:(fun acc src ->
        Topology.VertexSet.fold end_points ~init:acc
          ~f:(fun acc dst ->
            if src <> dst then SrcDstMap.add acc ~key:(src, dst) ~data:(paths src dst)
            else acc))

