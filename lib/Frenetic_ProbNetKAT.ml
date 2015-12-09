open Core.Std
open Frenetic_NetKAT

(* Outcomes of the probability space. An outcome is a map from coins to bool,
   indicating heads or tails
*)
module Omega = struct

  module T = Map.Make(Coin)

  type t = bool T.t (* coin -> bool, heads or tails *)

  (* SJS: this is really annoying... is there a way to include T, with type t monomorphic? *)
  let of_alist_exn = T.of_alist_exn
  let find = T.find

  let prob w =
    T.fold w ~init:1.0 ~f:(fun ~key:c ~data:heads acc ->
      acc *. (if heads then Coin.prob c else 1. -. Coin.prob c))

  let space_of_coins (cs : Coin.Set.t) : t list =
    let coins = Set.to_array cs in
    let rec loop n space =
      if n=0 then space else
      List.map space ~f:(List.cons true) @ List.map space ~f:(List.cons false)
      |> loop (n-1)
    in
    loop (Array.length coins) [[]]
    |> List.map ~f:(List.mapi ~f:(fun i b -> (coins.(i), b)))
    |> List.map ~f:of_alist_exn

  (* given a random variable [f] : Omega -> a, calculates the pushforward-measure on a *)
  let pushforward (type a) (type cmp) (coins : Coin.Set.t) ~(f : t -> a)
    ~(m : (module Map.S with type Key.t = a and type Key.comparator_witness = cmp))
    : (a, float, cmp) Map.t =
    let module Map = (val m) in
    space_of_coins coins
    |> List.map ~f:(fun w -> (f w, prob w))
    |> Map.of_alist_reduce ~f:(+.)

end


(* Internal Policy representation. Hash-consed modulo ACI. *)
module Pol = struct

  module Set = Int.Set
  module Map = Int.Map

  type policy =
    | Filter of header_val
    | Filter_out of header_val (* negated filter *)
    | Mod of header_val
    | Union of Set.t
    | Seq of int list
    | Choice of int * Coin.t * int
    | Star of int
    | Dup (* we can handle all of NetKAT *)
    (* | Deriv of FDK.t * FDK.t *)
  with sexp

  let compare_choice (x : int * Coin.t * int) (y : int * Coin.t * int) : int =
    Pervasives.compare x y

  let compare p1 p2 = match p1,p2 with
    | Filter hv1, Filter hv2
    | Filter_out hv1, Filter_out hv2
    | Mod hv1, Mod hv2 -> Pervasives.compare hv1 hv2
    | Union ps1, Union ps2 -> Set.compare ps1 ps2
    | Seq ps1, Seq ps2 -> List.compare Int.compare ps1 ps2
    | Choice (p1,c1,q1), Choice (p2,c2,q2) -> compare_choice (p1,c1,q1) (p2,c2,q2)
    | Star p1, Star p2 -> Int.compare p1 p2
    | Dup, Dup -> 0
    (* | Deriv (e0,d0), Deriv (e1,d1) -> Pervasives.compare (e0,d0) (e1,d1) *)
    | Filter _, _ -> -1
    | _, Filter _ -> 1
    | Filter_out _, _ -> -1
    | _, Filter_out _ -> 1
    | Mod _, _ -> -1
    | _, Mod _ -> 1
    | Union _, _ -> -1
    | _, Union _ -> 1
    | Seq _, _ -> -1
    | _, Seq _ -> 1
    | Choice _, _ -> -1
    | _, Choice _ -> 1
    | Star _, _ -> -1
    | _, Star _ -> 1
    (* | Dup, _ -> -1 *)
    (* | _, Dup -> 1 *)

  module T = Frenetic_Hashcons.Make(struct
    type t = policy with sexp
    let compare = compare
    let hash = Hashtbl.hash
  end)

  type t = int with sexp
  let get = T.get
  let unget = T.unget

  let drop = get (Union Set.empty)
  let id = get (Seq [])
  let dup = get Dup

  let mk_filter hv = get (Filter hv)
  let mk_filter_out hv = get (Filter_out hv)
  let mk_mod hv = get (Mod hv)

  let mk_union p1 p2 =
    match unget p1, Set.singleton p1, unget p2, Set.singleton p2 with
    | Union ps1, _, Union ps2, _
    | Union ps1, _, _, ps2
    | _, ps1, Union ps2, _
    | _, ps1, _, ps2
    -> Union (Set.union ps1 ps2) |> get

  let mk_seq p1 p2 =
    if p1 = drop || p2 = drop then drop else
    match unget p1, [p1], unget p2, [p2] with
    | Seq pl1, _, Seq pl2, _
    | Seq pl1, _, _, pl2
    | _, pl1, Seq pl2, _
    | _, pl1, _, pl2
    -> Seq (pl1 @ pl2) |> get

  let mk_or = mk_union
  let mk_and = mk_seq

  let mk_big_union = List.fold ~init:drop ~f:mk_union
  let mk_big_union' = Set.fold ~init:drop ~f:mk_union
  let mk_big_seq = List.fold_left ~init:id ~f:mk_seq

  let mk_choice p1 c p2 =
    if p1 = p2 then p1 else
    if Coin.prob c = 1.0 then p1 else
    if Coin.prob c = 0.0 then p2 else
    get (Choice (p1, c, p2))

  let mk_fresh_choice p1 p2 =
    let c = Coin.mk_fresh () in
    mk_choice p1 c p2

  let rec mk_star p =
    if p = drop || p = id then id else
    match unget p with
    | Star p -> mk_star p
    | Filter _
    | Filter_out _
    | Mod _ -> mk_union id p
    | x -> Star p |> get

  let rec of_pred ?(negate = false) (pred : Frenetic_NetKAT.pred) : t =
    match pred with
    | True when negate -> drop
    | True -> id
    | False when negate -> id
    | False -> drop
    | Test hv when negate -> mk_filter_out hv
    | Test hv -> mk_filter hv
    | And (p1, p2) when negate -> mk_or (of_pred ~negate p1) (of_pred ~negate p2)
    | And (p1, p2) -> mk_and (of_pred p1) (of_pred p2)
    | Or (p1, p2) when negate -> mk_and (of_pred ~negate p1) (of_pred ~negate p2)
    | Or (p1, p2) -> mk_or (of_pred p1) (of_pred p2)
    | Neg pred -> of_pred ~negate:(not negate) pred

  let match_loc sw pt =
    let t1 = mk_filter (Switch sw) in
    let t2 = mk_filter (Location (Physical pt)) in
    mk_seq t1 t2

  let mk_link ?(ing : Frenetic_NetKAT.pred option) s1 p1 s2 p2 =
    (* SJS: This is not the true sematnics of a link! This is a hack that works for now,
       but we will need to use the correct encoding once we start doing things like global
       optimization or deciding equivalence. *)
    let post_link = match ing with
      | None -> match_loc s2 p2
      | Some ing -> mk_seq (mk_filter (Switch s2)) (of_pred ~negate:true ing)
    in
    mk_big_seq [match_loc s1 p1; dup; post_link ]

  let rec of_pol ?(ing : Frenetic_NetKAT.pred option) (pol : Frenetic_NetKAT.policy) : t =
    match pol with
    | Filter a -> of_pred a
    | Mod hv -> mk_mod hv
    | Union (p,q) -> mk_union (of_pol ?ing p) (of_pol ?ing q)
    | Seq (p,q) -> mk_seq (of_pol ?ing p) (of_pol ?ing q)
    | Choice (p,c,q) -> mk_choice (of_pol ?ing p) c (of_pol ?ing q)
    | Star p -> mk_star (of_pol ?ing p)
    | Link (s1,p1,s2,p2) -> mk_link ?ing s1 p1 s2 p2
    | VLink _ -> assert false (* SJS / JNF *)

  let coins ?(acc=Coin.Set.empty) (t : t) : Coin.Set.t =
    let rec collect t acc =
      match unget t with
      | Filter _ | Filter_out _ | Mod _ | Dup -> acc
      | Choice (p,c,q) ->
        Coin.Set.add acc c
        |> collect p
        |> collect q
      | Union ps ->
        Set.fold_right ps ~init:acc ~f:collect
      | Seq pl ->
        List.fold_right pl ~init:acc ~f:collect
      | Star p ->
        collect p acc
    in
    collect t acc

  let rec resolve_choices t (w : Omega.t) =
    match unget t with
    | Filter _ | Filter_out _ | Mod _ | Dup -> t
    | Choice (p,c,q) ->
      begin match Omega.find w c with
      | None -> mk_choice (resolve_choices p w) c (resolve_choices q w)
      | Some heads -> if heads then resolve_choices p w else resolve_choices q w
      end
    | Union ps ->
      Set.map ps ~f:(fun t -> resolve_choices t w)
      |> mk_big_union'
    | Seq pl ->
      List.map pl ~f:(fun t -> resolve_choices t w)
      |> mk_big_seq
    | Star p ->
      mk_star (resolve_choices p w)

end



(* We need some extra operations on FDKs. *)
module FDK = struct
  include Frenetic_NetKAT_Compiler.FDK

  let rec of_local_pol (pol : Pol.t) =
    match Pol.unget pol with
    | Filter hv -> of_pred (Test hv)
    | Filter_out hv -> of_pred (Neg (Test hv))
    | Mod hv -> of_mod hv
    | Union ps ->
      Pol.Set.to_list ps
      |> List.map ~f:of_local_pol
      |> List.fold ~init:drop ~f:union
    | Seq pl ->
      List.map pl ~f:of_local_pol
      |> List.fold ~init:id ~f:seq
    | Star p -> star (of_local_pol p)
    | Choice _ -> failwith "expected deterministic policy, got probabilistic one"
    | Dup -> failwith "expected local policy, got global one"
end



(* syntactic Antimirov derivatives *)
module SynDeriv = struct

  type t = Pol.t * ((Pol.t * Pol.t) list)

  let drop = (Pol.drop, [])
  let id = (Pol.id, [])
  let dup = (Pol.drop, [(Pol.id, Pol.id)])

  let coins_in_hop (e, ds) =
    List.fold ds ~init:(Pol.coins e) ~f:(fun acc (d,k) -> Pol.coins ~acc d)

  let union (e1,ds1) (e2,ds2) =
    let e = Pol.mk_union e1 e2 in
    let ds = ds1 @ ds2 in
    (e, ds)

  let choice (e1,ds1) c (e2,ds2) =
    let open Pol in
    let e = mk_choice e1 c e2 in
    let ds1' = List.map ds1 ~f:(fun (d,k) -> (mk_choice d c drop, k)) in
    let ds2' = List.map ds2 ~f:(fun (d,k) -> (mk_choice drop c d, k)) in
    let ds = ds1' @ ds2' in
    (e, ds)

  let seq (e1,ds1) (p2, (e2,ds2)) =
    let e = Pol.mk_seq e1 e2 in
    let ds1' = List.map ds1 ~f:(fun (d,k) -> (d, Pol.mk_seq k p2)) in
    let ds2' = List.map ds2 ~f:(fun (d,k) -> (Pol.mk_seq e1 d, k)) in
    let ds = ds1' @ ds2' in
    (e, ds)

  let star p_star (e0,ds0) =
    let e = Pol.mk_star e0 in
    let ds = List.map ds0 ~f:(fun (d,k) -> (Pol.mk_seq e d, Pol.mk_seq k p_star)) in
    (e, ds)

  let rec of_pol pol =
    match Pol.unget pol with
    | Filter _
    | Filter_out _
    | Mod _ -> (pol, [])
    | Dup -> dup
    | Choice (p,c,q) -> choice (of_pol p) c (of_pol q)
    | Union ps ->
      Pol.Set.to_list ps
      |> List.map ~f:of_pol
      |> List.fold ~init:drop ~f:union
    | Seq pl ->
      List.map pl ~f:(fun p -> (p, of_pol p))
      |> List.fold ~init:id ~f:seq
    | Star p -> star pol (of_pol p)

  let resolve_choices (e,ds : t) (w : Omega.t) : t =
    let e = Pol.resolve_choices e w in
    let ds = List.map ds ~f:(fun (d,k) -> (Pol.resolve_choices d w, Pol.resolve_choices k w)) in
    (e, ds)
end


(* deterministic states *)
module DetState = struct
  type t = FDK.t * FDK.t with sexp

  let to_string ?(indent="") (e,d : t) : string =
    sprintf "%sE = %s\n%sD = %s" indent (FDK.to_string e) indent (FDK.to_string d)

  let compare : t -> t -> int = Pervasives.compare

  let zero = (FDK.drop, FDK.drop)
  let one = (FDK.id, FDK.drop)

  let union (e1,d1) (e2,d2) : t =
    (FDK.union e1 e2, FDK.union d1 d2)

  let conts (e,d : t) : Int.Set.t = FDK.conts d

  let map_conts (e,d : t) ~(f:int -> int) : t =
    (e, FDK.map_conts d ~f)

  let resolve_choices (t : t) (w : Omega.t) =
    map_conts t ~f:(fun k -> Pol.resolve_choices k w)

  let dependent_future_coins (t : t) : Coin.Set.t =
    let init = (Coin.Set.empty, Coin.Set.empty) (* (seen, dependent) *) in
    Set.fold (conts t) ~init ~f:(fun init k ->
      Set.fold (Pol.coins k) ~init ~f:(fun (seen, dependent) c ->
        if Set.mem seen c then (seen, Set.add dependent c)
        else (Set.add seen c, dependent)))
    |> snd

  let of_syn_deriv (e,ds : SynDeriv.t) : t =
    let e = FDK.of_local_pol e in
    let ds =
      List.map ds ~f:(fun (d,k) -> FDK.seq (FDK.of_local_pol d) (FDK.mk_cont k))
      |> List.fold ~init:FDK.drop ~f:FDK.union
    in
    (e, ds)

  let of_pol (pol : Pol.t) =
    of_syn_deriv (SynDeriv.of_pol pol)

  let of_syn_deriv_at_outcome (deriv : SynDeriv.t) (w : Omega.t) : t =
    SynDeriv.resolve_choices deriv w
    |> of_syn_deriv

  (* SJS: powerset construction, copied from global copiler *)
  let determinize (e,d : t) : t =
    let determinize_action par =
      let open Frenetic_NetKAT_Compiler in
      Par.to_list par
      |> List.sort ~cmp:Seq.compare_mod_k
      |> List.group ~break:(fun s1 s2 -> not (Seq.equal_mod_k s1 s2))
      |> List.map ~f:(function
        | [] -> assert false
        | [seq] -> seq
        | (rep::_ as group) ->
          let k =
            List.map group ~f:(fun s -> Seq.find_exn s K |> Value.to_int_exn)
            |> Pol.mk_big_union
          in
          Seq.add rep ~key:K ~data:(Value.of_int k))
      |> Par.of_list
    in
    (e, FDK.dedup d |> FDK.map_r determinize_action)

end



module ProbState = struct
  module Dist = struct
    include Map.Make(DetState) (* DetState.t -> float *)

    (* SJS: is there a better way to do this?? *)
    let map_keys m ~(f: Key.t -> Key.t) ~(merge: float -> float -> float) =
      to_alist m
      |> List.map ~f:(fun (k,v) -> (f k, v))
      |> of_alist_reduce ~f:merge
  end


  (* Invariant: values sum up to 1.0 *)
  type t = float Dist.t with sexp

  let to_string ?(indent="") (t : t) : string =
    Dist.to_alist t
    |> List.map ~f:(fun (st, p) ->
        sprintf "%s%f ->\n%s" indent p (DetState.to_string ~indent:(indent ^ "  ") st))
    |> String.concat ~sep:"\n"

  let hash t : int =
    Dist.to_alist t
    |> Hashtbl.hash

  let equal = Dist.equal Float.equal
  let compare = Dist.compare_direct Float.compare

  let dirac d = Dist.singleton d 1.0
  let drop = dirac DetState.zero
  let zero = drop
  let id = dirac DetState.one
  let one = id

  let choice ?(prob=0.5) (t1 : t) (t2 : t) =
    let t1 = Dist.map t1 ~f:(fun p -> p *. prob) in
    let t2 = Dist.map t2 ~f:(fun p -> p *. (1.0 -. prob)) in
    Dist.merge t1 t2 ~f:(fun ~key:_ -> function
      | `Left p | `Right p -> Some p
      | `Both (p1,p2) -> Some (p1 +. p2))

  let convolution t1 t2 ~(op:DetState.t -> DetState.t -> DetState.t) : t =
    Dist.fold t1 ~init:Dist.empty ~f:(fun ~key:d1 ~data:p1 acc ->
      Dist.fold t2 ~init:acc ~f:(fun ~key:d2 ~data:p2 acc ->
        let d = op d1 d2 in
        let p = p1 *. p2 in
        Dist.change acc d (function
          | None -> Some p
          | Some p' -> Some (p' +. p))))

  let union = convolution ~op:DetState.union

  let of_det_state = dirac

  let determinize (t : t) : t =
    Dist.map_keys t ~f:DetState.determinize ~merge:(+.)

  (* bring state into independent normal form, where all sucessor states
     (including the current state) are independent; this frees us from having
     to express all states as a joint distribution. *)
  let independent_normal_form (t : t) : t =
    (* SJS: determinize automaton to keep the number of dependend future coins minimal *)
    let t = determinize t in
    Dist.fold t ~init:Dist.empty ~f:(fun ~key:state ~data:prob acc ->
      DetState.dependent_future_coins state
      |> Omega.pushforward ~m:(module Dist) ~f:(fun w ->
          DetState.map_conts state ~f:(fun k -> Pol.resolve_choices k w))
      |> Dist.merge acc ~f:(fun ~key:_ -> function
        | `Left p1 -> Some p1
        | `Right p2 -> Some (p2 *. prob)
        | `Both (p1,p2) -> Some (p1 +. p2 *. prob)))

  let of_syn_deriv (e,ds : SynDeriv.t) : t =
    SynDeriv.coins_in_hop (e,ds)
    |> Omega.pushforward ~m:(module Dist) ~f:(DetState.of_syn_deriv_at_outcome (e,ds))
    |> independent_normal_form

  let of_pol (pol : Pol.t) : t =
    of_syn_deriv (SynDeriv.of_pol pol)

  let conts (t : t) : Int.Set.t =
    Dist.fold t ~init:Int.Set.empty ~f:(fun ~key:state ~data:prob acc ->
      (* SJS: we should really never have keys with probability 0 *)
      if prob = 0.0 then acc else
      DetState.conts state
      |> Set.union acc)

end



module Auto = struct

  module type Differentiable = sig
    type t with sexp
    val to_string : ?indent:string -> t -> string
    val of_pol : Pol.t -> t
    val conts : t -> Pol.Set.t
  end

  module Make(State : Differentiable) = struct

    type stateId = int with sexp
    type state = State.t

    type t = {
      start : stateId;
      states : State.t Int.Map.t;
    } with sexp

    let to_string (t : t) : string =
      Int.Map.to_alist t.states
      |> List.map ~f:(fun (id, state) ->
          sprintf "  s%d =\n%s" id (State.to_string ~indent:"    " state))
      |> List.cons (sprintf "\n\nSTART STATE = %d" t.start)
      |> String.concat ~sep:"\n"

    let of_pol' (pol : Pol.t) : t =
      let start = pol in
      let rec mk_states acc stateId : State.t Int.Map.t =
        if Map.mem acc stateId then acc else
        let state = State.of_pol stateId in
        let init = Map.add acc ~key:stateId ~data:state in
        let conts = State.conts state in
        Set.fold conts ~init ~f:mk_states
      in
      let states = mk_states Int.Map.empty start in
      { start; states }

    let of_pol (pol : Frenetic_NetKAT.policy) : t =
      of_pol' (Pol.of_pol pol)

  end

end

(* deterministic symbolic NetKAT automata *)
module DetAuto = Auto.Make(DetState)

(* probabilistic symbolic NetKAT automata *)
module ProbAuto = Auto.Make(ProbState)
