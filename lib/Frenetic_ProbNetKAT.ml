open Core.Std
open Frenetic_NetKAT
open Frenetic_NetKAT_Compiler

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

  let mk_big_union = List.fold_left ~init:drop ~f:mk_union
  let mk_big_seq = List.fold_left ~init:id ~f:mk_seq

  let mk_choice p c q =
    get (Choice (p,c,q))

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
    | Star p -> mk_star (of_pol ?ing p)
    | Link (s1,p1,s2,p2) -> mk_link ?ing s1 p1 s2 p2
    | VLink _ -> assert false (* SJS / JNF *)

  let coins ?(acc=[]) t =
    let rec collect t acc =
      match unget t with
      | Filter _ | Filter_out _ | Mod _ | Dup -> acc
      | Choice (p,c,q) ->
        c :: acc
        |> collect p
        |> collect q
      | Union ps ->
        Set.to_list ps
        |> List.fold_right ~init:acc ~f:collect
      | Seq pl ->
        List.fold_right pl ~init:acc ~f:collect
      | Star p ->
        collect p acc
    in
    collect t acc
end


(* syntactic Antimirov derivatives *)
module SynDiv = struct

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
    | Star p -> star pol (of_pol pol)

end


module State = struct
  type t = FDK.t * FDK.t with sexp
  let compare = Pervasives.compare

  let zero = (FDK.drop, FDK.drop)
  let one = (FDK.id, FDK.drop)

  let union (e1,d1) (e2,d2) =
    (FDK.union e1 e2, FDK.union d1 d2)

end


module FDK_Dist = struct
  module Dom = State
  module Dist = Map.Make(Dom)

  (* Invariant: values sum up to 1.0 *)
  type t = float Dist.t with sexp

  let hash t : int =
    Dist.to_alist t |> List.map ~f:(fun (o,p) -> (Hashtbl.hash o, p)) |> Hashtbl.hash

  let equal = Dist.equal Float.equal
  let compare = Dist.compare_direct Float.compare

  let dirac d = Dist.singleton d 1.0
  let zero = dirac Dom.zero
  let one = dirac Dom.one

  let convolution t1 t2 ~(op:Dom.t -> Dom.t -> Dom.t) : t =
    Dist.fold t1 ~init:Dist.empty ~f:(fun ~key:d1 ~data:p1 acc ->
      Dist.fold t2 ~init:acc ~f:(fun ~key:d2 ~data:p2 acc ->
        let d = op d1 d2 in
        let p = p1 *. p2 in
        Dist.change acc d (function
          | None -> Some p
          | Some p' -> Some (p' +. p))))

  let union = convolution ~op:Dom.union

(*   let choice ?(prop=0.5) p c q =
    let p' = Dist.map p ~f:(fun pr -> prop *. pr) in
    let q' = Dist.map q ~f:(fun pr -> (1.0 -. prop) *. pr) in
    union p' q'
 *)
(*
  (* This is the correct ProbNetKAT star semantics:
     p* = 1 + p0 + p0;p1 + p0;p1;p2 + ... *)
  let star t =
    let rec loop acc power =
      let acc' = union acc power in
      if equal acc acc'
        then acc
        else loop acc' (seq power t)
    in loop id t

  let of_pred ?(negate=false) pred =
    let pred = if negate then Neg pred else pred in
    dirac (D.of_pred pred)

  let of_mod hv =
    dirac (D.of_mod hv)

(*   let of_cont p =
    Pol.of_pol p |> FDK.mk_cont |> dirac *)

  let rec of_local_pol (pol : Frenetic_NetKAT.policy) =
    match pol with
    | Filter pred -> of_pred pred
    | Mod hv -> of_mod hv
    | Union (p, q) -> union (of_local_pol p) (of_local_pol q)
    | Seq (p, q) -> seq (of_local_pol p) (of_local_pol q)
    | Star p -> star (of_local_pol p)
    | Link _
    | VLink _ -> failwith "Expected local policy, but found link!"

end


(* let seq_with_pol (t:t) (p:Pol.t) =
  FDK_Dist.(Dist.fold t ~init:Dist.empty ~f:(fun ~key:fdk ~data:prob acc ->
    let fdk' =
      FDK.map_r (Par.fold ~init:Par.empty ~f:(fun acc seq -> Par.add acc (Seq.change seq K (function
        | Some (Const k) -> Some (Const (Pol.mk_seq (Int.of_int64_exn k) q |> Int64.of_int))
        | _ -> failwith "continuation expected - none found"))))
      fdk
    in
    Dist.change acc fdk' (function
      | None -> Some prob
      | Some prob' -> Some (prob' +. prob)))) *)
*)

end
