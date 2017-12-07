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
  val counter : string (* failure counter *)
  val up : int -> int -> string

  (* switch term for a particular switch *)
  val sw_pol : [ `Switchwise of Topo.vertex -> string policy
               | `Portwise   of Topo.vertex -> int -> string policy
               ]

  (* probability of link failure for particular link (sw,pt) *)
  val failure_prob : int -> int -> Prob.t

  val max_failures : int option

  (* the topology *)
  val topo : Net.Topology.t
  val destination : int
end) = struct

  include Params

  let rec make () : string policy =
    let ingress = Topology.ingress topo ~dst:destination in
    PNK.(
      (if Option.is_none max_failures then skip else !!(counter, 0)) >>
      (* in; (¬eg; p; t)*; eg *)
      filter ingress >>
      whl (neg (???(sw, destination))) (
        hop ()
      )
    )

  and hop () =
    let open PNK in
    Topology.locs topo
    |> List.filter ~f:(fun (sw,_pt) -> Topology.sw_val topo sw <> destination)
    |> ite_cascade ~otherwise:drop ~f:(fun (sw, pts) ->
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
    match max_failures with
    | None ->
      List.map pts ~f:(fun pt ->
        let p = failure_prob sw pt in
        ?@[ !!(up sw pt, 0) @ p; skip @ Prob.(one - p) ]
      )
      |> mk_big_seq
    | Some bound ->
      List.map pts ~f:(fun pt ->
        List.range 0 bound ~start:`inclusive ~stop:`exclusive
        |> ite_cascade ~otherwise:skip ~f:(fun f ->
          let p = failure_prob sw pt in
          let guard = ???(counter, f) in
          let body =
            ?@[ !!(up sw pt, 0) >> !!(counter, f+1) , p;
                skip                                , Prob.(one - p);
              ]
          in
          (guard, body)
        )
      )
      |> mk_big_seq

  (** teleport from ingress straight to destination  *)
  let teleportation () =
    PNK.(
      filter (Topology.ingress topo ~dst:destination) >>
      !!(sw, destination)
    )

end
