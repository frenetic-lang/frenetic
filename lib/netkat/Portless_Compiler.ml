open Core
open Syntax

module Network = Frenetic_kernel.Network
module Net = Network.Net
module Topology = Net.Topology

type direction = Incoming | Outgoing
type topo = (abstract_location, Topology.vertex) Hashtbl.t * Topology.t

let connected_switches ~loc ~direction ~topo =
  let (name_to_vertex_table, network) = topo in
  let vertex = Hashtbl.find name_to_vertex_table loc in
  match vertex with
  | None -> failwith "no such location"
  | Some loc ->
    let neighbors = Topology.neighbors network loc in
    Topology.VertexSet.fold neighbors ~init:[] ~f:(fun acc other ->
        let node = Topology.vertex_to_label network other in
        if Network.Node.device node = Switch then
          let edges = match direction with
            | Incoming -> Topology.find_all_edges network other loc
            | Outgoing -> Topology.find_all_edges network loc other in
          Topology.EdgeSet.fold edges ~init:acc ~f:(fun acc edge ->
              let v, port =  match direction with
                | Incoming -> Topology.edge_src edge
                | Outgoing -> Topology.edge_dst edge in
              (Network.Node.id (Topology.vertex_to_label network other), port) :: acc)
        else
          acc)

let abs_loc_to_switch (topo: topo) loc =
  let (name_to_vertex_table, network) = topo in
  let vertex = Hashtbl.find name_to_vertex_table loc in
  match vertex with
  | Some v ->
    let node = (Topology.vertex_to_label network v) in
    if Network.Node.device node = Switch then
      Network.Node.id node
    else
      failwith "the location is not a switch"
  | None -> failwith "no such location"

let is_loc_host (topo: topo) loc =
  let (name_to_vertex_table, network) = topo in
  let vertex = Hashtbl.find name_to_vertex_table loc in
  match vertex with
  | Some v -> Network.Node.device (Topology.vertex_to_label network v) = Host
  | None -> failwith "no such location"


let portify_pred pred (topo: topo) =
  let rec portify_pred' pred (k: pred -> pred) =
    match pred with
    | True -> k True
    | False -> k False
    | And (pred1, pred2) ->
      portify_pred' pred1 (fun x ->
          portify_pred' pred2 (fun y ->
              k (And (x, y))))
    | Or (pred1, pred2) ->
      portify_pred' pred1 (fun x ->
          portify_pred' pred2 (fun y ->
              k (Or (x, y))))
    | Neg pred ->
      portify_pred' pred (fun x -> k (Neg x))
    | Test header -> match header with
      | AbstractLoc loc -> k (Test (Switch (abs_loc_to_switch topo loc)))
      | From loc ->
        let from_list = connected_switches loc Incoming topo in
        List.fold from_list
          ~init:(False)
          ~f:(fun acc (sw, pt) ->
              k (Or (acc, And (
                  (Test (Switch sw)),
                  (Test (Location (Physical pt)))))))
      | Switch _ | Location _ -> failwith "cannot specify switch and port for portless policies"
      | x -> k (Test x) in
  portify_pred' pred (fun x -> x)

let portify_pol_fdd (portless_pol_fdd: Local_compiler.t) (topo: topo): policy =
  let rec portify_pol' portless_pol k =
    match portless_pol with
    | Union (pol1, pol2) ->
      portify_pol' pol1 (fun x ->
          portify_pol' pol2 (fun y ->
              k (Union (x, y))))
    | Seq (pol1, pol2) ->
      portify_pol' pol1 (fun x ->
          portify_pol' pol2 (fun y ->
              k (Seq (x, y))))
    | Star pol -> portify_pol' pol (fun x -> k (Star x))
    | Filter pred -> k (Filter (portify_pred pred topo))
    | Let meta -> portify_pol' meta.body (fun x -> k (Let {meta with body = x}))
    | Dup -> k Dup
    | Link _ | VLink _ -> failwith "links not supported for portless policies"
    | Mod header -> match header with
      | AbstractLoc loc ->
        let sw_port_list = connected_switches loc Outgoing topo in
        k (List.fold sw_port_list
             ~init:(if is_loc_host topo loc then drop else Filter (Test (Switch (abs_loc_to_switch topo loc))))
             ~f:(fun acc (sw, mod_pt) ->
                 let portful_test = Test (Switch sw) in
                 let portful_mod = Mod (Location (Physical mod_pt)) in
                 Union (acc, Seq (Filter portful_test, portful_mod))))
      | From loc -> k id
      | Switch _ | Location _ -> failwith "cannot specify switch and port for portless policies"
      | x -> k (Mod x) in
  let portless_pol = Local_compiler.to_local_pol portless_pol_fdd in
  portify_pol' portless_pol (fun x -> x)

let make_topo (network: Topology.t): topo =
  let name_to_vertex_table = String.Table.create () in
  Topology.iter_vertexes (fun vertex ->
      let vertex_name = (Network.Node.name (Topology.vertex_to_label network vertex)) in
      let _ = Hashtbl.add name_to_vertex_table vertex_name vertex in ()) network;
  (name_to_vertex_table, network)

let compile portless_pol (network: Topology.t) =
  let topo = make_topo network in
  let portless_pol_fdd = Local_compiler.compile portless_pol in
  portify_pol_fdd portless_pol_fdd topo
