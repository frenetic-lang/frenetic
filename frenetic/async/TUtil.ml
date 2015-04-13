(* Topology utility functions. This module should eventually be replaced with a
 * Frenetic-specific topology module that includes the ocaml-topology module.
 *)

open Core.Std

module Net = Async_NetKAT.Net
module SDN = SDN_Types

let switch_ids (t : Net.Topology.t) : SDN.switchId list =
  let open Net.Topology in
  fold_vertexes (fun v acc ->
    match vertex_to_label t v with
    | Async_NetKAT.Switch id -> id::acc
    | _ -> acc)
  t []

(* Topology detection doesn't really detect hosts. So, I treat any
   port not connected to a known switch as an edge port *)
let internal_ports (t : Net.Topology.t) (sw_id : SDN.switchId) =
  let open Net.Topology in
  let open Async_NetKAT in
  let switch = vertex_of_label t (Switch sw_id) in
  PortSet.fold  (vertex_to_ports t switch) ~init:PortSet.empty ~f:(fun acc p ->
    match next_hop t switch p with
    | Some e ->
       let node, _ = edge_dst e in
       begin match vertex_to_label t node with
       | Switch _ -> PortSet.add acc p
       | _ -> acc
       end
    | _ -> acc)

let in_edge (t : Net.Topology.t) (sw_id : SDN.switchId) (pt_id : SDN.portId) =
  let open Net.Topology in
  let open Async_NetKAT in
  let switch = vertex_of_label t (Switch sw_id) in
  match next_hop t switch pt_id with
  | None    -> true
  | Some(_) -> false

let edge (t: Net.Topology.t) =
  let open Net.Topology in
  fold_vertexes (fun v acc ->
    match vertex_to_label t v with
    | Async_NetKAT.Switch sw_id ->
      PortSet.fold (vertex_to_ports t v) ~init:acc ~f:(fun acc pt_id ->
        match next_hop t v pt_id with
        | None   -> (sw_id, pt_id)::acc
        | Some _ -> acc)
    | _ -> acc)
  t []
