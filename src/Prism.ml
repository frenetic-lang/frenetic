open Core
open Syntax
open Symbolic

let state = "__state__"

module Transition = struct
  type value = [`Int of int | `Field of string] [@@deriving hash, sexp, compare]
  type t = value Base.Map.M(String).t [@@deriving hash, sexp, compare]
  let equal x y = compare x y = 0
  let pp _ = failwith "not implemented"
  let to_string _ = failwith "not implemented"
  let skip : t = Base.Map.empty (module String)
  let of_mod (f, v) : t = Base.Map.singleton (module String) f (`Int v)
  let to_state s : t = Base.Map.singleton (module String) state (`Int s)
end

module TransitionDist = struct
  include Dist.Make(Transition)
  let none : t = dirac Transition.skip
end

module Automaton = struct

  type rule = {
    guard : string pred;
    transitions : TransitionDist.t;
  }

  type state = rule list

  let dummy_state = [{ guard = True; transitions = TransitionDist.none}]

  type t = state Int.Map.t

  let add_state (t : t) (s : state) : (int * t) =
    let key = Map.max_elt t |> Option.map ~f:fst |> Option.value ~default:0 in
    (key, Map.add_exn t ~key ~data:s)
end


let thompson (p : string policy) : Automaton.t * int * int list =
  let wire (auto : Automaton.t) (src : int) (dst : int) : Automaton.t =
    Map.update auto src ~f:(function
      | None ->
        assert false
      | Some rules ->
        List.map rules ~f:(fun rule ->
          let transitions =
            TransitionDist.pushforward rule.transitions ~f:(fun trans ->
              Base.Map.add_exn trans ~key:state ~data:(`Int dst)
            )
          in
          { rule with transitions}
        )
      )
  in
  let rec thompson p auto : Automaton.t * int * int list =
    match p with
    | Filter pred ->
      let (state, auto) =
        Automaton.add_state auto [{
          guard = pred;
          transitions = TransitionDist.none
        }]
      in
      (auto, state, [state])
    | Modify hv ->
      let (state, auto) =
        Automaton.add_state auto [{
          guard = True;
          transitions = TransitionDist.dirac (Transition.of_mod hv)
        }]
      in
      (auto, state, [state])
    | Seq (p, q) ->
      let (auto, start_p, final_p) = thompson p auto in
      let (auto, start_q, final_q) = thompson q auto in
      let auto =
        List.fold final_p ~init:auto ~f:(fun auto state ->
          wire auto state start_q
        )
      in
      (auto, start_p, final_q)
    | Ite (a, p, q) ->
      let (auto, start_p, final_p) = thompson p auto in
      let (auto, start_q, final_q) = thompson q auto in
      let rules = Automaton.[
          { guard = a;
            transitions = TransitionDist.dirac (Transition.to_state start_p) };
          { guard = Neg a;
            transitions = TransitionDist.dirac (Transition.to_state start_q) };
        ]
      in
      let (state, auto) = Automaton.add_state auto rules in
      (auto, state, final_p @ final_q)
    | While (a, p) ->
      let (auto, start_p, final_p) = thompson p auto in
      let (final, auto) = Automaton.(add_state auto dummy_state) in
      let (start, auto) = Automaton.add_state auto [
          { guard = a;
            transitions = TransitionDist.dirac (Transition.to_state start_p) };
          { guard = Neg a;
            transitions = TransitionDist.dirac (Transition.to_state final) };
        ]
      in
      (auto, start, [final])
    | Choice (dist : (string policy * Prob.t) list) ->
      let (pols, probs) = List.unzip dist in
      let auto, wires = List.fold_map pols ~init:auto ~f:(fun auto p ->
          let (auto, start, final) = thompson p auto in
          (auto, (start, final))
        )
      in
      let (starts, finals) = List.unzip wires in
      let transitions =
        List.zip_exn (List.map starts ~f:Transition.to_state) probs
        |> TransitionDist.of_alist_exn
      in
      let (start, auto) = Automaton.add_state auto [{
          guard = True;
          transitions;
        }]
      in
      (auto, start, List.concat finals)
    | Let { id; init; body; _ } ->
      let (auto, start_body, final_body) = thompson body auto in

      (* initialize local field *)
      let (start, auto) = Automaton.add_state auto [{
          guard = True;
          transitions =
            Transition.to_state start_body
            |> Base.Map.add_exn ~key:id ~data:(
              match init with
              | Alias field -> `Field field
              | Const n -> `Int n
            )
            |> TransitionDist.dirac
        }]
      in

      (* set local field to zero when it goes out of scope *)
      let (final, auto) = Automaton.add_state auto [{
          guard = True;
          transitions = TransitionDist.dirac (Transition.of_mod (id, 0));
        }]
      in

      let auto = List.fold final_body ~init:auto ~f:(fun a s -> wire a s final) in
      (auto, start, [final])

    (* | ObserveUpon of 'field policy * 'field pred (* exexcute policy, then observe pred *) *)
  in
  thompson p Int.Map.empty



let dopped = "dropped"

(* let of_input_dist = *)

let of_pol p ~(input_dist : Packet.Dist.t) =
  failwith "not implemented"
