(** Module for generating ProbNetKAT network model in the style (p;t)*;p.

    TODO: probably need a parameter to get partial model (where while loop
    is unrolled a final number of times).
*)
open! Core
open Syntax
module Net = Frenetic.Network.Net
module Node = Frenetic.Network.Node
module Topo = Net.Topology

(* statically evaluate all up bit tests to True *)
let delete_up_bits p =
  let open Syntax.Constructors in
  let rec do_pred a =
    match a with
    | Test (f,v) when Params.is_up_field f ->
      True
    | Test _ | True | False ->
      a
    | And (a,b) ->
      conj (do_pred a) (do_pred b)
    | Or (a,b) ->
      disj (do_pred a) (do_pred b)
    | Neg a ->
      neg (do_pred a)
  in
  let rec do_pol p =
    match p with
    | Filter a ->
      Filter (do_pred a)
    | Modify (f,v) when Params.is_up_field f ->
      skip
    | Modify _ ->
      p
    | Seq (p,q) ->
      seq (do_pol p) (do_pol q)
    | Ite (a,p,q) ->
      ite (do_pred a) (do_pol p) (do_pol q)
    | While (a,p) ->
      whl (do_pred a) (do_pol p)
    | Branch {branches; parallelize} ->
      branch ~parallelize (Util.map_fst branches ~f:do_pred |> Util.map_snd ~f:do_pol)
    | Choice choices ->
      choice (Util.map_fst choices ~f:do_pol)
    | Let { id; init; mut; body } ->
      Let { id; init; mut; body = do_pol body }
    | ObserveUpon (p, a) ->
      ObserveUpon (do_pol p, do_pred a)
(*     | Repeat (n,p) ->
      Repeat (n, do_pol p) *)
  in
  do_pol p


let make
  ?(bound : int option)
  ~(failure_prob : int -> int -> Prob.t)
  ~(max_failures : int option)
  ~(sw_pol : Schemes.scheme)
  ~(topo : Net.Topology.t)
  ()
  =

  let open Params in
  let no_failures =
    max_failures = Some 0 || List.for_all (Topology.locs topo) ~f:(fun (sw,pts) ->
      List.for_all pts ~f:(fun pt ->
        failure_prob (Topology.sw_val topo sw) pt
        |> Prob.(equal zero)
      )
    )
  in

  let rec make () : string policy =
    let ingress = Topology.ingress topo ~dst:destination in
    PNK.(
      (if Option.is_none max_failures || no_failures then skip else !!(counter, 0)) >>
      (* in; (¬eg; p; t)*; eg *)
      filter ingress >>
      match bound with
      | None ->
        whl (neg (???(sw, destination))) (
          hop ()
        )
      | Some bound ->
        !!(ttl, bound) >>
        whl (neg (???(sw, destination))) (
          hop () >>
          begin
            List.range 1 bound ~stop:`inclusive
            |> ite_cascade ~disjoint:true ~otherwise:drop ~f:(fun ttl_val ->
              ???(ttl, ttl_val), !!(ttl, ttl_val - 1)
            )
          end
        )
    )



  and hop () =
    let open PNK in
    Topology.locs' topo
    |> List.filter ~f:(fun (sw,_,_) -> Topology.sw_val topo sw <> destination)
    |> ite_cascade ~disjoint:true ~otherwise:drop ~f:(fun (sw, host_pts, sw_pts) ->
      let sw_id = Topology.sw_val topo sw in
      let guard = ???(Params.sw, sw_id) in
      let body =
        with_init_up_bits sw_id sw_pts @@ begin
          begin match sw_pol with
          | `Switchwise pol ->
            pol sw
          | `Portwise pol ->
            (* want rules for both ports connecting to hosts and ports connecting
               to other switches
             *)
            List.append host_pts sw_pts
            |> ite_cascade ~disjoint:true ~otherwise:drop ~f:(fun pt_id ->
              (???(pt, pt_id), pol sw pt_id)
            )
          end
          |> (if no_failures then delete_up_bits else ident)
          >>
          (* SJS: a subtle point: we model only switch-to-switch links! *)
          Topology.links_from topo sw
            ~guard_links:(not no_failures)
            ~dst_filter:(Topology.is_switch topo)
        end

      in
      (guard, body)
    )

  and with_init_up_bits sw pts body =
    if no_failures then
      body
    else
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

  in
  make ()

(** teleport from ingress straight to destination  *)
let teleportation topo =
  PNK.(
    filter (Topology.ingress topo ~dst:Params.destination) >>
    !!(Params.sw, Params.destination)
  )
