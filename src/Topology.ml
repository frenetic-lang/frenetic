(** Module for generating ProbNetKAT encoding of topology with certain failure
    model.
*)
open! Core
open Syntax
open Params

module Net = Frenetic.Network.Net
open Net.Topology
module Node = Frenetic.Network.Node


let parse : string -> Net.Topology.t =
  Net.Parse.from_dotfile


let is_switch topo v =
  match Node.device (vertex_to_label topo v) with
  | Node.Switch -> true
  | Node.Middlebox | Node.Host -> false

let is_host topo v =
  match Node.device (vertex_to_label topo v) with
  | Node.Host -> true
  | Node.Middlebox | Node.Switch -> false

let is_core topo v =
  if is_switch topo v then
  match Node.level (vertex_to_label topo v) with
    | Node.Core -> true
    | _ -> false
  else false

let is_agg topo v =
  if is_switch topo v then
  match Node.level (vertex_to_label topo v) with
    | Node.Aggregation -> true
    | _ -> false
  else false


let switches topo : vertex list =
  vertexes topo
  |> Set.filter ~f:(is_switch topo)
  |> Set.to_list
  (* |> List.map ~f:(Topology.sw_val topo) *)

let hosts topo : vertex list =
  vertexes topo
  |> Set.filter ~f:(is_host topo)
  |> Set.to_list

let pt_val pt : int =
  Int.of_int32_exn pt

let sw_val topo sw : int =
  assert (is_switch topo sw);
  vertex_to_label topo sw
  |> Node.id
  |> Int.of_int64_exn

let host_val topo host : int =
  assert (is_host topo host);
  vertex_to_label topo host
  |> Node.id
  |> Int.of_int64_exn

let vertex_to_ports ~dst_filter topo vertex =
  neighbors topo vertex
  |> Set.filter ~f:dst_filter
  |> Set.to_list
  |> List.map ~f:(fun neigb -> find_edge topo vertex neigb)
  |> List.map ~f:edge_src
  |> List.map ~f:(fun (_,src_pt) -> src_pt)


let core_agg_locs topo : (int * int) list =
  fold_edges (fun edge acc ->
      let (src_vertex,src_pt) = edge_src edge in
      let (dst_vertex,dst_pt) = edge_dst edge in
      if (is_core topo src_vertex) && (is_agg topo dst_vertex) then
        (sw_val topo src_vertex, pt_val src_pt) :: acc
      else
        acc
    )
   topo []


let locs topo : (vertex * int list) list =
  List.map (switches topo) ~f:(fun sw ->
    vertex_to_ports ~dst_filter:(fun _ -> true) topo sw
    |> List.map ~f:pt_val
    |> (fun pts -> (sw, pts))
  )

(* return (sw, host-connected-ports, switch-connected-ports) triples  *)
let locs' topo : (vertex * int list * int list) list =
  List.map (switches topo) ~f:(fun sw ->
    let host_ports = vertex_to_ports topo sw ~dst_filter:(is_host topo) in
    let sw_ports = vertex_to_ports topo sw ~dst_filter:(is_switch topo) in
    (sw, List.map host_ports ~f:pt_val, List.map sw_ports ~f:pt_val)
  )

let ingress_locs ?(dst: int option) topo : (vertex * int) list =
  fold_edges (fun edge acc ->
      let (src_vertex,src_pt) = edge_src edge in
      let (dst_sw,dst_pt) = edge_dst edge in
      if not (is_host topo src_vertex) || Some (sw_val topo dst_sw) = dst then
        acc
      else
        (dst_sw, pt_val dst_pt) :: acc
    )
      topo
      []

let link_of_edge (topo : Net.Topology.t) edge ~(guard:bool) : (string pred * string policy) =
  let (src_sw,src_pt) = edge_src edge in
  let (dst_sw,dst_pt) = edge_dst edge in
  let src_sw = sw_val topo src_sw in
  let dst_sw = sw_val topo dst_sw in
  let src_pt = pt_val src_pt in
  let dst_pt = pt_val dst_pt in
  let guard = PNK.(
    ???(sw, src_sw) & ???(pt, src_pt) &
    (if guard then ???(up src_sw src_pt, 1) else True)
  )
  in
  (guard, PNK.( !!(sw, dst_sw) >> !!(pt, dst_pt) ))

let raw_links_from ?(dst_filter=fun _ -> true)
(topo : Net.Topology.t) sw ~(guard_links: bool) : (string pred * string policy) list =
  neighbors topo sw
  |> Set.filter ~f:dst_filter
  |> Set.to_list
  |> List.map ~f:(find_edge topo sw)
  |> List.map ~f:(link_of_edge topo ~guard:guard_links)

let links_from ?(dst_filter=fun _ -> true)
(topo : Net.Topology.t) sw ~(guard_links: bool) : string policy =
  raw_links_from ~dst_filter topo sw ~guard_links
  |> PNK.(mk_big_ite ~disjoint:true ~default:drop)

let to_probnetkat (topo : Net.Topology.t) ~(guard_links:bool) : string policy =
  switches topo
  |> List.concat_map ~f:(raw_links_from topo ~guard_links ~dst_filter:(is_switch topo))
  |> PNK.(mk_big_ite ~disjoint:true ~default:drop)

let ingress ?(dst: int option) (topo : Net.Topology.t) : string pred =
  ingress_locs topo ?dst
  |> Util.map_fst ~f:(sw_val topo)
  |> List.map ~f:(fun (sw_id, pt_id) ->
      PNK.( ???(sw, sw_id) & ???(pt, pt_id))
    )
  |> PNK.mk_big_disj

let uniform_ingress ?(dst: int option) (topo : Net.Topology.t) : Symbolic.Packet.Dist.t =
  let open Symbolic in
  let sw = Fdd.abstract_field Params.sw in
  let pt = Fdd.abstract_field Params.pt in
  ingress_locs topo ?dst
  |> Util.map_fst ~f:(sw_val topo)
  |> Util.map_both ~f:(fun v -> PrePacket.Const v)
  |> List.map ~f:(fun (sw_v, pt_v) ->
    Packet.(modify (modify empty sw sw_v) pt pt_v)
  )
  |> Packet.Dist.uniform

let uniform_ingress' (topo : Net.Topology.t) ~(dst: int) : Symbolic.Fdd.t =
  let open Syntax in
  ingress_locs topo ~dst
  |> Util.map_fst ~f:(sw_val topo)
  |> List.map ~f:(fun (sw_v, pt_v) ->
    PNK.(!!(Params.sw, sw_v) >> !!(Params.pt, pt_v))
  )
  |> PNK.uniform
  |> Symbolic.Fdd.of_pol
