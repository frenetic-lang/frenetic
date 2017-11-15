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

  (* probability of link failure for particular link (sw,pt) *)
  val failure_prob : int -> int -> Prob.t

  (* the topology *)
  val topo : Net.Topology.t
  val eggress : string pred
end) = struct

  include Params
  module Topology = Topology.Make(Params)

  let rec make () : string policy =
    PNK.(
      filter (Topology.ingress topo) >>
      whl (neg eggress) (
        (* p; t *)
        hop ~final:false ()
      ) >>
      (* p *)
      hop ~final:true ()
    )

  and hop ~final () =
    let open PNK in
    ite_cascade (Topology.locs topo) ~otherwise:drop ~f:(fun (sw, pts) ->
      let sw_id = Topology.sw_val topo sw in
      let guard = ???(Params.sw, sw_id) in
      let body =
        (if final then ident else with_init_up_bits sw_id pts) begin
          ite_cascade pts ~otherwise:drop ~f:(fun pt_id ->
            (???(pt, pt_id), for_loc sw_id pt_id)
          )
          >>
          if final then skip else Topology.links_from topo sw ~guard_links:true
        end

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


module DefaultParams = struct
  let sw = "sw"
  let pt = "pt"
  let up _sw pt = sprintf "up_%d" pt
end
