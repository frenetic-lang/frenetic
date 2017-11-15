(** Module for generating ProbNetKAT encoding of topology with certain failure
    model.
*)
open! Core
open Syntax

module Net = Frenetic.Network.Net
open Net.Topology

let parse : string -> Net.Topology.t =
  Net.Parse.from_dotfile

let pt_val pt : int =
  Int.of_int32_exn pt

let sw_val topo sw : int =
  vertex_to_string topo sw
  (* |> Util.tap ~f:(eprintf "switch \"%s\"\n") *)
  |> Int.of_string

module Make(Parameters: sig
  val sw : string
  val pt : string
  val up : int -> int -> string
end) = struct

  open Parameters

  let to_probnetkat (topo : Net.Topology.t) ~(guard_links:bool) : string policy =
    fold_edges (fun edge t ->
        let (src_sw,src_pt) = edge_src edge in
        let (dst_sw,dst_pt) = edge_dst edge in
        let src_sw = sw_val topo src_sw in
        let dst_sw = sw_val topo dst_sw in
        let src_pt = pt_val src_pt in
        let dst_pt = pt_val dst_pt in
        let guard = 
          if guard_links then PNK.(???(up src_sw src_pt, 1)) else False
        in
        PNK.(
          ite ( ???(sw, src_sw) & ???(pt, src_pt) & guard ) (
            !!(sw, dst_pt) >> !!(pt, dst_pt)
          ) (
            t
          )
        )
      )
      topo
      PNK.drop


end
