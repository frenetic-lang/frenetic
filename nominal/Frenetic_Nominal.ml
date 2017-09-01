open Core
open Frenetic_NetKAT_Compiler

module NetKAT_Auto = Frenetic_NetKAT_Compiler.Automaton
module Par = Action.Par
module Seq = Action.Seq

module Automaton = struct

  type constr =
  | Eq of int * int (* Eq n m: field number n is equal to value number m *)
  | Neq of int * int (* Neq n m: field number n is not equal to value number m *)
  | MkEq of int * int
  [@@deriving sexp, yojson]

  type edge = int * constr list * int
    [@@deriving sexp, yojson]

  type finalEdge = int * constr list
    [@@deriving sexp, yojson]

  type t = {
    (* integer >=2; by convention, 0 is the start state, n-1 = nr_of_states-1 is
       the unique accepting state, and 1,...,n-2 are the remaining states *)
    nr_of_states : int;
    nr_of_constants : int; (* integer >= 0 *)
    nr_of_fields : int; (* integer >= 0 *)
    edges : edge list;
    final_edges : finalEdge list;
  } [@@deriving sexp, yojson]

  let cont_of_seq seq =
    match Seq.find seq Action.K with
    | Some Value.Const k -> Int.of_int64_exn k
    | _ -> failwith "invalid continuation"

  (* normalize automaton to use states 0 up to n-1 *)
  let normalize_states (auto : NetKAT_Auto.t) : NetKAT_Auto.t =
    (* maps old state ids to new state ids *)
    let state_map : (int, int) Hashtbl.t = Int.Table.create () in
    (* next unused state id *)
    let next = ref 0 in
    let new_auto = NetKAT_Auto.create_t () in
    NetKAT_Auto.iter_reachable auto ~order:`Pre ~f:(fun id _ ->
      Hashtbl.add_exn state_map ~key:id ~data:(!next);
      incr next);
    NetKAT_Auto.iter_reachable auto ~f:(fun id (e,d) ->
      let id = Hashtbl.find_exn state_map id in
      let d = FDD.map_conts d ~f:(Hashtbl.find_exn state_map) in
      NetKAT_Auto.add_to_t_with_id new_auto (e,d) id);
    assert (new_auto.source = Hashtbl.find_exn state_map auto.source);
    new_auto

  (* fold over all FDDs in a NetKAT automaton *)
  let auto_fold ~init ~ftest ~fmod auto =
    NetKAT_Auto.fold_reachable auto ~init ~f:(fun init _ (e,d) ->
      let init = FDD.deep_fold e ~init ~ftest ~fmod in
      FDD.deep_fold d ~init ~ftest ~fmod)

  let of_netkat_auto (auto : NetKAT_Auto.t) =
    let auto = normalize_states auto in

    let fv_pairs = 
      let f ~init fv = match fv with 
        | (f, Value.Const v) -> (f,v) :: init
        | _ -> failwith "not supported" in
      auto_fold auto ~init:[] ~ftest:f ~fmod:f in

    let fields = 
      List.map fv_pairs ~f:fst
      |> List.map ~f:(Field.to_enum) 
      |> Int.Set.of_list in

    let constants =
      List.map fv_pairs ~f:snd
      |> List.map ~f:Int.of_int64_exn
      |> Int.Set.of_list in

    let nr_of_states = List.fold (Hashtbl.keys auto.states) ~init:0 ~f:max + 1 in
    let nr_of_constants = Set.length constants in
    let nr_of_fields = Set.length fields in

    let field_map : int Int.Map.t =
      Int.Set.to_list fields
      |> List.mapi ~f:(fun i f -> (f, i))
      |> Int.Map.of_alist_exn in

    let constant_map : int Int.Map.t =
      Int.Set.to_list constants
      |> List.mapi ~f:(fun i c -> (c, i))
      |> Int.Map.of_alist_exn in

    let (edges, final_edges) =
      let rec of_state id (e,d) =
        (FDD.fold d ~f:(of_leaf ~src:id) ~g:of_branch,
         FDD.fold e ~f:(of_final_leaf ~src:id) ~g:of_final_branch)
      and of_leaf ~src par =
        List.map ~f:(of_seq ~src) (Par.to_list par)
      and of_final_leaf ~src par =
        List.map ~f:(of_final_seq ~src) (Par.to_list par)
      and of_seq ~src seq =
        let dst = cont_of_seq seq in
        let cnstrs = 
          Seq.to_hvs seq
          |> List.map ~f:normalize_hv
          |> List.map ~f:(fun (f,v) -> MkEq (f,v)) in
        (src, cnstrs, dst)
      and of_final_seq ~src seq =
        let cnstrs = 
          Seq.to_hvs seq
          |> List.map ~f:normalize_hv
          |> List.map ~f:(fun (f,v) -> MkEq (f,v)) in
        (src, cnstrs)
      and of_branch (f,v) es es' =
        let f,v = normalize_hv (f, v) in
        List.map es ~f:(fun (s, cs, d) -> (s, Eq (f,v) :: cs, d))
        @ List.map es' ~f:(fun (s, cs, d) -> (s, Neq (f,v) :: cs, d))
      and of_final_branch (f,v) es es' =
        let f,v = normalize_hv (f, v) in
        List.map es ~f:(fun (s, cs) -> (s, Eq (f,v) :: cs))
        @ List.map es' ~f:(fun (s, cs) -> (s, Neq (f,v) :: cs)) 
      and normalize_hv (f,v) =
        let f = Map.find_exn field_map (Field.to_enum f) in
        match v with
        | Value.Const v -> (f, Map.find_exn constant_map (Int.of_int64_exn v))
        | _ -> assert false
      in

      NetKAT_Auto.fold_reachable auto ~init:([],[]) ~f:(fun acc id fdds ->
        let (es, fes) = of_state id fdds in
        let (es', fes') = acc in
        (es @ es', fes @ fes'))
    in

    { nr_of_states; nr_of_constants; nr_of_fields; edges; final_edges }

    let of_pol p =
      NetKAT_Auto.of_policy ~dedup:true ~cheap_minimize:true p
      |> of_netkat_auto
end
