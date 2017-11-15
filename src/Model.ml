(** Module for generating ProbNetKAT network model in the style (p;t)*;p.

    TODO: probably need a parameter to get partial model (where while loop
    is unrolled a final number of times).
*)
open! Core
open Syntax
module Net = Frenetic.Network.Net
module Node = Frenetic.Network.Node
module Topo = Net.Topology

module Make(Params: sig
  val sw : string
  val pt : string
  val up : int -> int -> string

  (* switch and topo terms for a particular switch *)
  val sw_pol : int -> int -> string policy
  val topo_pol : int -> string policy

  (* probability of link failure for particular link (sw,pt) *)
  val failure_prob : int -> int -> Prob.t

  (* the topology *)
  val topo : Net.Topology.t
  val ingress : string pred
  val eggress : string pred
end) = struct

include Params

let switches =
  Topo.vertexes topo
  |> Set.filter ~f:(fun v -> match Node.device (Topo.vertex_to_label topo v) with
      | Node.Switch -> true
      | Node.Middlebox | Node.Host -> false
  )
  |> Set.to_list
  (* |> List.map ~f:(Topology.sw_val topo) *)

let locs : (int * int list) list =
  List.map switches ~f:(fun sw ->
    let pts =
      Topo.vertex_to_ports topo sw
      |> Set.to_list
      |> List.map ~f:Topology.pt_val
    in
    (Topology.sw_val topo sw, pts)
  )

let rec make () : string policy =
  PNK.(
    filter ingress >>
    whl (neg eggress) (
      hop ~final:false ()
    ) >>
    hop ~final:true ()
  )

and hop ~final () =
  let open PNK in
  ite_cascade locs ~otherwise:drop ~f:(fun (sw_id, pts) ->
    let guard = ???(sw, sw_id) in
    let body =
      (if final then ident else with_init_up_bits sw_id pts) @@
      ite_cascade pts ~otherwise:drop ~f:(fun pt_id ->
        (???(pt, pt_id), for_loc sw_id pt_id)
      )
    in
    (guard, body)
  )

and for_loc sw pt =
  Params.sw_pol sw pt

and with_init_up_bits sw pts body =
  let up_bits = List.map pts ~f:(fun pt -> (up sw pt, 0, true)) in
  PNK.locals up_bits body

and init_up_bits sw pts =
  let open PNK in
  List.map pts ~f:(fun pt ->
    let p = failure_prob sw pt in
    ?@[ !!(up sw pt, 1) @ p; !!(up sw pt, 0) @ Prob.(one - p) ]
  )
  |> mk_big_seq



end
