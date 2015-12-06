open Core.Std
open Frenetic_NetKAT

module Pol = struct

  type policy =
    | Filter of header_val
    | Filter_out of header_val (* negated filter *)
    | Mod of header_val
    | Union of Int.Set.t
    | Seq of int list
    | Choice of float Int.Map.t
    | Star of int
    | Dup (* we can handle all of NetKAT *)
  with sexp

  let compare p1 p2 = match p1,p2 with
    | Filter hv1, Filter hv2
    | Filter_out hv1, Filter_out hv2
    | Mod hv1, Mod hv2 -> compare hv1 hv2
    | Union ps1, Union ps2 -> Int.Set.compare ps1 ps2
    | Seq ps1, Seq ps2 -> List.compare Int.compare ps1 ps2
    | Choice d1, Choice d2 -> Int.Map.compare Float.compare d1 d2
    | Star p1, Star p2 -> Int.compare p1 p2
    | Dup, Dup -> 0
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

  module T = Frenetic_Hashcons.Make(struct
    type t = policy with sexp
    let compare = compare
    (* SJS: this may not be safe!!!! Choice is an Int.Map *)
    let hash = Hashtbl.hash
  end)

  type t = int
  let get = T.get
  let unget = T.unget

  let drop = get (Union Int.Set.empty)
  let id = get (Seq [])
  let dup = get Dup

  let mk_filter hv = get (Filter hv)
  let mk_filter_out hv = get (Filter_out hv)
  let mk_mod hv = get (Mod hv)

  let mk_union p1 p2 =
    match unget p1, Int.Set.singleton p1, unget p2, Int.Set.singleton p2 with
    | Union ps1, _, Union ps2, _
    | Union ps1, _, _, ps2
    | _, ps1, Union ps2, _
    | _, ps1, _, ps2
    -> Union (Int.Set.union ps1 ps2) |> get

  let mk_seq p1 p2 =
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

  let mk_choice pl =
    match pl with
    | [] -> failwith "not a valid distribution"
    | [p] -> p
    | _ ->
      let prob = 1.0 /. Float.of_int (List.length pl) in
      List.map pl ~f:(fun p -> (p, prob))
      |> Int.Map.of_alist
      |> function
          | `Duplicate_key _ -> failwith "not a valid distribution"
          | `Ok d -> get (Choice d)

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
end

module DFDK = struct
  module D = Frenetic_NetKAT_Compiler
  module M = Map.Make(D)

  (* Invariant: values sum up to 1.0 *)
  type t = float M.t

  let hash t =
    M.to_alist t |> List.map ~f:(fun (o,p) -> (Hashtbl.hash o, p))

  let equal = M.equal Float.equal
  let compare = M.compare_direct Float.compare

  let dirac a = M.singleton a 1.0
  let drop = dirac D.drop
  let id = dirac D.id

  let convolution t1 t2 ~(op:D.t -> D.t -> D.t) : t =
    M.fold t1 ~init:M.empty ~f:(fun ~key:o1 ~data:p1 acc ->
      M.fold t2 ~init:acc ~f:(fun ~key:o2 ~data:p2 acc ->
        let o = op o1 o2 in
        let p = p1 *. p2 in
        M.change acc o (function
          | None -> Some p
          | Some p' -> Some (p' +. p))))

  let union = convolution ~op:D.union
  let seq = convolution ~op:D.seq

  (* This is the correct ProbNetKAT star semantics:
     p* = 1 + p0 + p0;p1 + p0;p1;p2 + ... *)
  let star t =
    let rec loop acc power =
      let acc' = union acc power in
      if equal acc acc'
        then acc
        else loop acc' (seq power t)
    in loop id t

  let of_pred pred =
    dirac (D.of_pred pred)

  let of_mod hv =
    dirac (D.of_mod hv)

  let rec of_policy pol =
    match pol with
    | Filter pred -> of_pred pred
    | Mod hv -> of_mod hv
    | Union (p, q) -> union (of_policy p) (of_policy q)
    | Seq (p, q) -> seq (of_policy p) (of_policy q)
    | Star p -> star (of_policy p)
    | Link _
    | VLink _ -> failwith "Expected local policy, but found link!"

end
