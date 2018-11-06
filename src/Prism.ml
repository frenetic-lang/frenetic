open Core
open Syntax
open Symbolic

module Transition = struct
  type t = int Base.Map.M(String).t [@@deriving hash, sexp, compare]
  let equal x y = compare x y = 0
  let pp _ = failwith "not implemented"
  let to_string _ = failwith "not implemented"
  let skip : t = Base.Map.empty (module String)
  let of_mod (f, v) : t = Base.Map.singleton (module String) f v
end

module TransitionDist = struct
  include Dist.Make(Transition)
  let none : t = dirac Transition.skip
end

module Automaton = struct

  type state = {
    guard : string pred;
    transitions : TransitionDist.t;
  }

  type t = state Int.Map.t

  let add_state (t : t) (s : state) : (int * t) =
    let key = Map.max_elt t |> Option.map ~f:fst |> Option.value ~default:0 in
    (key, Map.add_exn t ~key ~data:s)
end


let tompson (p : string policy) : Automaton.t * int * int list =
  let state = "__state__" in
  let rec tompson p auto : Automaton.t * int * int list =
    match p with
    | Filter pred ->
      let (state, auto) =
        Automaton.add_state auto {
          guard = pred;
          transitions = TransitionDist.none
        }
      in
      (auto, state, [state])
    | Modify hv ->
      let (state, auto) =
        Automaton.add_state auto {
          guard = True;
          transitions = TransitionDist.dirac (Transition.of_mod hv)
        }
      in
      (auto, state, [state])
    | Seq (p, q) ->
      let (auto, start_p, final_p) = tompson p auto in
      let (auto, start_q, final_q) = tompson q auto in
      let auto =
        List.fold final_p ~init:auto ~f:(fun auto state_id ->
          Map.update auto state_id ~f:(function
            | None ->
              assert false
            | Some {guard; transitions } ->
              let transitions =
                TransitionDist.pushforward transitions ~f:(fun trans ->
                  Base.Map.add_exn trans ~key:state ~data:start_q
                )
              in
              { guard; transitions}
          )
        )
      in
      (auto, start_p, final_q)

    (* | Ite of 'field pred * 'field policy * 'field policy *)
    (* | While of 'field pred * 'field policy *)
    (* | Choice of ('field policy * Prob.t) list *)
    (* | Let of { id : 'field; init : 'field meta_init; mut : bool; body : 'field policy } *)
    (* | ObserveUpon of 'field policy * 'field pred (* exexcute policy, then observe pred *) *)
  in
  tompson p Int.Map.empty



let dopped = "dropped"

(* let of_input_dist = *)

let of_pol p ~(input_dist : Packet.Dist.t) =
  failwith "not implemented"
