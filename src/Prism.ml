open Core
open Syntax
open Symbolic

(* SJS/FIXME: ensure this field is unused by user program. *)
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

  let drop_state = 0
  let empty = Int.Map.singleton drop_state [{
    guard = True;
    transitions = TransitionDist.dirac (Transition.to_state drop_state)
  }]

  let add_state (t : t) (s : state) : (int * t) =
    let key =
      Map.max_elt t
      |> function
        | None -> 0
        | Some (key, _) -> key + 1
    in
    (key, Map.add_exn t ~key ~data:s)
end

let wire (auto : Automaton.t) (src : int) (dst : int) : Automaton.t =
  Map.update auto src ~f:(function
    | None ->
      assert false
    | Some rules ->
      List.map rules ~f:(fun rule ->
        let transitions =
          TransitionDist.pushforward rule.transitions ~f:(fun trans ->
            Base.Map.update trans state ~f:(function
              | None -> `Int dst
              | Some v -> v
            )
            (* Base.Map.add_exn trans ~key:state ~data:(`Int dst) *)
          )
        in
        { rule with transitions}
      )
    )

let thompson (p : string policy) : Automaton.t * int * int list =
  let rec thompson p auto : Automaton.t * int * int list =
    match p with
    | Filter pred ->
      let (state, auto) =
        Automaton.add_state auto [
          { guard = pred;
            transitions = TransitionDist.none };
          { guard = Neg pred;
            transitions = TransitionDist.dirac (Transition.to_state Automaton.drop_state) };
        ]
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
      (* SJS/FIXME: ensure id is "fresh"! *)
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
    | ObserveUpon (p, a) ->
      (* SJS/FIXME: ensure that p is idempotent! *)
      thompson PNK.(do_whl (Neg a) p) auto

  in
  thompson p Automaton.empty


type input_dist = ((string * int) list * Prob.t) list

let auto_of_pol p ~(input_dist : input_dist) : int * Automaton.t * int =
  let (auto, start', final') = thompson p in
  (* add start state *)
  let transitions =
    Util.map_fst input_dist ~f:(fun assignments ->
      (state, start') :: assignments
      |> Util.map_snd ~f:(fun n -> `Int n)
      |> Base.Map.of_alist_exn (module String)
    )
    |> TransitionDist.of_alist_exn
  in
  let (start, auto) = Automaton.add_state auto [{ guard = True; transitions }] in
  (* add final state *)
  let (final, auto) = Automaton.(add_state auto dummy_state) in
  let auto = List.fold final' ~init:auto ~f:(fun a s -> wire a s final) in
  let auto = wire auto final final in
  (start, auto, final)


(* for each field, the set of values used with that field *)
module Domain = struct
  type t = (Int.Set.t) String.Map.t

  let add (t : t) (f,v : string header_val) =
    Map.update t f ~f:(function
      | None -> Int.Set.singleton v
      | Some s -> Set.add s v
    )

  let merge : t -> t -> t =
    Map.merge_skewed ~combine:(fun ~key -> Int.Set.union)
end



let dom_of_pol (p : string policy) : Domain.t =
  let rec do_pol p ((dom : Domain.t), (locals : (string * field meta_init) list) as acc) =
    match p with
    | Filter pred -> acc
    | Modify hv -> (Domain.add dom hv, locals)
    | Seq (p, q) -> acc |> do_pol p |> do_pol q
    | Ite (a, p, q) -> acc |> do_pred a |> do_pol p |> do_pol q
    | While (a, p)
    | ObserveUpon (p, a) -> acc |> do_pred a |> do_pol p
    | Choice ps -> List.fold ps ~init:acc ~f:(fun acc (p,_) -> do_pol p acc)
    | Let { id; init; body; _ } -> (dom, (id,init)::locals) |> do_pol body
  and do_pred a (dom, locals as acc) =
    (* FIXME *)
    acc
  in
  let (dom, locals) = do_pol p (String.Map.empty, []) in
  List.fold locals ~init:dom ~f:(fun dom (f, init) ->
    match init with
    | Const v ->
      Domain.add dom (f, v)
    | Alias g ->
      begin match Map.find dom g with
      | None -> dom
      | Some vs -> Map.update dom f ~f:(function
        | None -> vs
        | Some vs' -> Set.union vs vs')
      end
  )


let of_pol p ~(input_dist : input_dist) : string =
  let (start, auto, final) = auto_of_pol p ~input_dist in
  let subst = Int.Table.create () in
  (* conventions *)
  Hashtbl.add_exn subst ~key:Automaton.drop_state ~data:0;
  failwith "todo"

