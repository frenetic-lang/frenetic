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

  (* switch term for a particular switch *)
  val sw_pol : [ `Switchwise of Topo.vertex -> string policy
               | `Portwise   of Topo.vertex -> int -> string policy
               ]

  (* probability of link failure for particular link (sw,pt) *)
  val failure_prob : int -> int -> Prob.t

  (* the topology *)
  val topo : Net.Topology.t
  val destination : string pred
end) = struct

  include Params
  module Topology = Topology.Make(Params)

  let rec make () : string policy =
    PNK.(
      (* in; (¬eg; p; t)*; eg *)
      filter (Topology.ingress topo) >>
      whl (neg destination) (
        hop ()
      )
    )

  and hop () =
    let open PNK in
    ite_cascade (Topology.locs topo) ~otherwise:drop ~f:(fun (sw, pts) ->
      let sw_id = Topology.sw_val topo sw in
      let guard = ???(Params.sw, sw_id) in
      let body =
        with_init_up_bits sw_id pts @@ begin
          begin match sw_pol with
          | `Switchwise pol ->
            pol sw
          | `Portwise pol ->
            ite_cascade pts ~otherwise:drop ~f:(fun pt_id ->
              (???(pt, pt_id), pol sw pt_id)
            )
          end
          >>
          (* SJS: a subtle point: we model only switch-to-switch links! *)
          Topology.links_from topo sw ~guard_links:true ~dst_filter:(Topology.is_switch topo)
        end

      in
      (guard, body)
    )

  and with_init_up_bits sw pts body =
    let up_bits = List.map pts ~f:(fun pt -> (up sw pt, 1, true)) in
    PNK.locals up_bits PNK.(init_up_bits sw pts >> body)

  and init_up_bits sw pts =
    let open PNK in
    List.map pts ~f:(fun pt ->
      let p = failure_prob sw pt in
      ?@[ !!(up sw pt, 0) @ p; skip @ Prob.(one - p) ]
    )
    |> mk_big_seq

  (** teleport from ingress straight to destination  *)
  let teleportation () =
    PNK.(
      filter (Topology.ingress topo) >>
      Syntax.positive_pred_to_mod destination
    )

end
