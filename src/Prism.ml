open Core
open Syntax

type input_dist = ((string * int) list * Prob.t) list

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

  let wire (auto : t) (src : int) (dst : int) : t =
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

  let wire_all (auto : t) (srcs : int list) (dst : int) : t =
    List.fold srcs ~init:auto ~f:(fun auto state ->
      wire auto state dst
    )

  let thompson (p : string policy) : t * int * int list =
    let rec thompson p auto : t * int * int list =
      match p with
      | Filter pred ->
        let (state, auto) =
          add_state auto [
            { guard = pred;
              transitions = TransitionDist.none };
            { guard = PNK.neg pred;
              transitions = TransitionDist.dirac (Transition.to_state drop_state) };
          ]
        in
        (auto, state, [state])
      | Modify hv ->
        let (state, auto) =
          add_state auto [{
            guard = True;
            transitions = TransitionDist.dirac (Transition.of_mod hv)
          }]
        in
        (auto, state, [state])
      | Seq (p, q) ->
        let (auto, start_p, final_p) = thompson p auto in
        let (auto, start_q, final_q) = thompson q auto in
        let auto = wire_all auto final_p start_q in
        (auto, start_p, final_q)
      | Ite _ ->
        thompson (Branch (Syntax.branches p)) auto
      | Branch branches ->
        let (start, auto) = add_state auto [] in
        let auto, branches =
          List.fold_map branches ~init:auto ~f:(fun auto (guard, p) ->
            let (auto, start, final) = thompson p auto in
            (auto, (guard, start, final))
          )
        in
        let rules = List.map branches ~f:(fun (guard, start, _) ->
          let transitions = TransitionDist.dirac (Transition.to_state start) in
          { guard; transitions }
        )
        in
        let auto = Map.set auto ~key:start ~data:rules in
        let final = List.concat_map branches ~f:(fun (_, _, final) -> final) in
        (auto, start, final)
      | While (a, p) ->
        let (start, auto) = add_state auto [] in
        let (auto, start_p, final_p) = thompson p auto in
        let (final, auto) = (add_state auto dummy_state) in
        let auto = Map.set auto ~key:start ~data:[
            { guard = a;
              transitions = TransitionDist.dirac (Transition.to_state start_p) };
            { guard = PNK.neg a;
              transitions = TransitionDist.dirac (Transition.to_state final) };
          ]
        in
        let auto = wire_all auto final_p start in
        (auto, start, [final])
      | Choice (dist : (string policy * Prob.t) list) ->
        let (start, auto) = add_state auto [] in
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
        let auto = Map.set auto ~key:start ~data:[{
            guard = True;
            transitions;
          }]
        in
        (auto, start, List.concat finals)
      | Let { id; init; body; _ } ->
        (* SJS/FIXME: ensure id is "fresh"! *)
        let (start, auto) = add_state auto [] in
        let (auto, start_body, final_body) = thompson body auto in
        (* initialize local field *)
        let auto = Map.set auto ~key:start ~data:[{
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
        let (final, auto) = add_state auto [{
            guard = True;
            transitions = TransitionDist.dirac (Transition.of_mod (id, 0));
          }]
        in
        let auto = List.fold final_body ~init:auto ~f:(fun a s -> wire a s final) in
        (auto, start, [final])
      | ObserveUpon (p, a) ->
        (* SJS/FIXME: ensure that p is idempotent! *)
        thompson PNK.(do_whl (neg a) p) auto

    in
    thompson p empty


  let of_pol p ~(input_dist : input_dist) : int * t * int =
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
    let (start, auto) = add_state auto [{ guard = True; transitions }] in
    (* add final state *)
    let (final, auto) = add_state auto dummy_state in
    let auto = List.fold final' ~init:auto ~f:(fun a s -> wire a s final) in
    let auto = wire auto final final in
    (start, auto, final)
end



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

  let of_pol (p : string policy) : t =
    let rec do_pol p ((dom : t), (locals : (string * field meta_init) list) as acc) =
      match p with
      | Filter pred -> do_pred pred acc
      | Modify hv -> (add dom hv, locals)
      | Seq (p, q) -> acc |> do_pol p |> do_pol q
      | Ite (a, p, q) -> acc |> do_pred a |> do_pol p |> do_pol q
      | Branch ps -> List.fold ps ~init:acc ~f:(fun acc (a,p) ->
        acc |> do_pred a |> do_pol p )
      | While (a, p)
      | ObserveUpon (p, a) -> acc |> do_pred a |> do_pol p
      | Choice ps -> List.fold ps ~init:acc ~f:(fun acc (p,_) -> do_pol p acc)
      | Let { id; init; body; _ } -> (dom, (id,init)::locals) |> do_pol body
    and do_pred a (dom, locals as acc) =
      match a with
      | True
      | False -> acc
      | Test hv -> (add dom hv, locals)
      | And (a, b)
      | Or (a, b) -> acc |> do_pred a |> do_pred b
      | Neg a -> do_pred a acc
    in
    let (dom, locals) = do_pol p (String.Map.empty, []) in
    List.fold locals ~init:dom ~f:(fun dom (field, init) ->
      match init with
      | Const v ->
        add dom (field, v)
      | Alias g ->
        begin match Map.find dom g with
        | None -> dom
        | Some vs -> Map.update dom field ~f:(function
          | None -> vs
          | Some vs' -> Set.union vs vs')
        end
    )
end


(** {2 PRISM AST} *)
module Ast = struct

  type field_decl = {
    name : string;
    lower_bound : int;
    upper_bound : int;
    init : int option;
  }

  type model = {
    decls : field_decl list;
    rules : Automaton.rule list;
  }

  let decls_of_domain (dom : Domain.t) : field_decl list =
    Map.to_alist dom
    |> List.map ~f:(fun (field, vals) ->
      { name = field;
        lower_bound = Set.min_elt_exn vals;
        upper_bound = Set.max_elt_exn vals;
        init = None;
      }
    )

  let rule_of_auto_rule ({ guard; transitions } : Automaton.rule)
    (state_id : int) (subst : (int, int) Hashtbl.t) : Automaton.rule =
    let guard = PNK.(test (state, state_id) & guard) in
    (* rename states *)
    let transitions =
      TransitionDist.pushforward transitions ~f:(fun t ->
        Base.Map.change t state ~f:(function
          | None -> None
          | Some (`Int s) -> Some (`Int (Hashtbl.find_exn subst s))
          | Some _ -> failwith "invalid transition"
        )
      )
    in
    { guard; transitions }

  let rules_of_auto (auto : Automaton.t) (subst : (int, int) Hashtbl.t) : Automaton.rule list =
    Map.to_alist auto
    |> Util.map_fst ~f:(fun state_id -> Hashtbl.find_exn subst state_id)
    |> List.sort ~compare:(fun (s, _) (t, _) -> Int.compare s t)
    |> List.concat_map ~f:(fun (state_id, rules) ->
      List.map rules ~f:(fun rule ->
        rule_of_auto_rule rule state_id subst
      )
    )
    |> List.filter ~f:(function { guard = False; _ } -> false | _ -> true)

  let model_of_pol p ~(input_dist : input_dist) : model =
    let (start, auto, final) = Automaton.of_pol p ~input_dist in
    let state_subst = Int.Table.create () in
    (* rename states *)
    Hashtbl.add_exn state_subst ~key:Automaton.drop_state ~data:(-1);
    Hashtbl.add_exn state_subst ~key:final ~data:0;
    Hashtbl.add_exn state_subst ~key:start ~data:1;
    Map.keys auto
    |> List.filter ~f:(fun id -> id <> Automaton.drop_state && id <> final && id <> start)
    |> List.sort ~compare:Int.compare
    |> List.iteri ~f:(fun i old_id ->
        Hashtbl.add_exn state_subst ~key:old_id ~data:(i + 2)
    );

    let state_decl =
      { name = state;
        lower_bound = -1;
        upper_bound = Map.length auto - 2;
        init = Some (Hashtbl.find_exn state_subst start) }
    in
    let decls = state_decl :: decls_of_domain (Domain.of_pol p) in
    let rules = rules_of_auto auto state_subst in
    { decls; rules}
end


(** {2 Code generation} *)
module Code = struct

  let indent ?(spaces=2) s =
    String.split_lines s
    |> List.map ~f:((^) (String.make spaces ' '))
    |> String.concat ~sep:"\n"

  let preamble = "dtmc\n\nmodule ProbNetKAT\n"
  let postamble = "endmodule"

  let of_many ?(sep="\n") of_one many =
    List.map many ~f:of_one
    |> String.concat ~sep

  let of_init = function
    | None -> ""
    | Some v -> Format.sprintf " init %d" v

  let of_decl Ast.{ name; lower_bound; upper_bound; init } =
    Format.sprintf "%s : [%d..%d]%s;"
      name lower_bound upper_bound (of_init init)

  let of_value = function
    | `Int n -> Int.to_string n
    | `Field f -> f

  let of_prob prob =
    match Prob.to_int_frac prob with
    | 0, _ -> "0"
    | 1,1 -> "1"
    | num, den -> Format.sprintf "%d/%d" num den

  let of_pred (a : string pred) =
    let rec do_pred a =
      match a with
      | True -> "true"
      | False -> "false"
      | Test (f,v) -> Format.sprintf "%s=%d" f v
      | Neg (Test (f,v)) -> Format.sprintf "%s!=%d" f v
      | Neg a -> Format.sprintf "!(%s)" (do_pred a)
      | And _ -> of_many do_conjunct (conjuncts a) ~sep:" & "
      | Or _ -> of_many do_pred (disjuncts a) ~sep:" | "
    and do_conjunct = function
      | Or _ as a -> Format.sprintf "(%s)" (do_pred a)
      | a -> do_pred a
    in
    do_pred a

  let of_mod (field, value) =
    Format.sprintf "(%s'=%s)" field (of_value value)

  let of_transition (mods, prob) =
    Format.sprintf "%s : %s"
      (of_prob prob)
      (of_many of_mod ~sep:" & " (Base.Map.to_alist mods))

  let of_rule Automaton.{ guard; transitions } =
    Format.sprintf "[] %s -> %s;"
      (of_pred guard)
      (of_many of_transition (TransitionDist.to_alist transitions) ~sep:" + ")

  let of_model (model : Ast.model) : string =
    Format.sprintf "%s\n%s\n\n%s\n\n%s\n"
      preamble
      (of_many of_decl model.decls |> indent)
      (of_many of_rule model.rules |> indent)
      postamble

  let of_pol p ~(input_dist : input_dist) : string =
    Ast.model_of_pol p ~input_dist
    |> of_model

end

