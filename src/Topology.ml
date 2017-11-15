(** Module for generating ProbNetKAT encoding of topology with certain failure
    model.
*)
open! Core
open Syntax

module Net = Frenetic.Network.Net
open Net.Topology
module Node = Frenetic.Network.Node


(** things that don't need to be parameterized  *)
module Generic = struct
let parse : string -> Net.Topology.t =
  Net.Parse.from_dotfile

let pt_val pt : int =
  Int.of_int32_exn pt

let sw_val topo sw : int =
  vertex_to_string topo sw
  (* |> Util.tap ~f:(eprintf "switch \"%s\"\n") *)
  |> Int.of_string


let switches topo : vertex list =
  vertexes topo
  |> Set.filter ~f:(fun v -> match Node.device (vertex_to_label topo v) with
      | Node.Switch -> true
      | Node.Middlebox | Node.Host -> false
  )
  |> Set.to_list
  (* |> List.map ~f:(Topology.sw_val topo) *)

let is_host topo v =
  match Node.device (vertex_to_label topo v) with
  | Node.Host -> true
  | Node.Middlebox | Node.Switch -> false

let locs topo : (vertex * int list) list =
  List.map (switches topo) ~f:(fun sw ->
    let pts =
      vertex_to_ports topo sw
      |> Set.to_list
      |> List.map ~f:pt_val
    in
    (sw, pts)
  )

end

include Generic


(** make this a functor, so we can parameterize over various configurations  *)
module Make(Parameters: sig
  val sw : string
  val pt : string
  val up : int -> int -> string
end) = struct

  open Parameters
  include Generic

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
    (guard, PNK.( !!(sw, dst_pt) >> !!(pt, dst_pt) ))

  let to_probnetkat (topo : Net.Topology.t) ~(guard_links:bool) : string policy =
    fold_edges (fun edge t ->
        let (guard, action) = link_of_edge topo edge ~guard:guard_links in
        PNK.ite guard action t
      )
      topo
      PNK.drop

  let links_from (topo : Net.Topology.t) sw ~(guard_links: bool) : string policy =
    neighbors topo sw
    |> Set.to_list
    |> List.map ~f:(find_edge topo sw)
    |> List.map ~f:(link_of_edge topo ~guard:guard_links)
    |> PNK.(mk_big_ite ~default:drop)

  let ingress (topo : Net.Topology.t) : string pred =
    fold_edges (fun edge acc ->
      let (src_vertex,src_pt) = edge_src edge in
      if not (is_host topo src_vertex) then acc else
      let (dst_sw,dst_pt) = edge_dst edge in
      let dst_sw = sw_val topo dst_sw in
      let dst_pt = pt_val dst_pt in
      PNK.(disj acc (???(sw, dst_sw) & ???(pt, dst_pt)))
    )
      topo
      False


end
