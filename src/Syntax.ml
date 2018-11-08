open Core

[@@@ocaml.warning "-30"]

let fprintf = Format.fprintf


(** {2} fields and values *)
type field = string [@@deriving sexp, show, compare, eq, hash]
type value = int [@@deriving sexp, show, compare, eq, hash]

type 'field header_val = 'field * value [@@deriving sexp, compare, eq, hash]


(** {2} predicates and policies *)

(* local/meta fields *)
type 'field meta_init =
  | Alias of 'field
  | Const of value
  [@@deriving sexp, compare, hash]

type 'field pred =
  | True
  | False
  | Test of 'field header_val
  | And of 'field pred * 'field pred
  | Or of 'field pred * 'field pred
  | Neg of 'field pred
  [@@deriving sexp, compare, hash]

type 'field  policy =
  | Filter of 'field pred
  | Modify of 'field header_val
  | Seq of 'field policy * 'field policy
  | Ite of 'field pred * 'field policy * 'field policy
  | Branch of ('field pred * 'field policy) list
  | While of 'field pred * 'field policy
  | Choice of ('field policy * Prob.t) list
  | Let of { id : 'field; init : 'field meta_init; mut : bool; body : 'field policy }
  | ObserveUpon of 'field policy * 'field pred (* exexcute policy, then observe pred *)
  (* | Repeat of int * 'field policy *)
  [@@deriving sexp, compare, hash]

(** negation normal form *)
let nnf (a : 'field pred) : 'field pred =
  let rec nnf a negate =
    match a with
    | True when negate -> False
    | False when negate -> True
    | Test _ when negate -> Neg a
    | And (a, b) when negate -> Or (nnf a negate, nnf b negate)
    | Or (a, b) when negate -> And (nnf a negate, nnf b negate)
    | Neg a -> nnf a (not negate)
    | True | False | Test _ -> a
    | And (a, b) -> And (nnf a negate, nnf b negate)
    | Or (a, b) -> Or (nnf a negate, nnf b negate)
  in
  nnf a false

(** disjunctive normal form *)
let dnf (a : 'field pred) : (('field header_val * [`Eq | `Neq]) list) list =
  let rec nnf_to_dnf a =
    match a with
    | True ->
      [[]]
    | False ->
      []
    | Test hv ->
      [[(hv, `Eq)]]
    | Neg (Test hv) ->
      [[(hv, `Neq)]]
    | Neg _ ->
      failwith "not in NNF"
    | Or (a,b) ->
      nnf_to_dnf a @ nnf_to_dnf b
    | And (a,b) ->
      List.cartesian_product (nnf_to_dnf a) (nnf_to_dnf b)
      |> List.map ~f:(fun (ai, bi) -> ai @ bi)
  in
  nnf_to_dnf (nnf a)

let conjuncts a =
  let exception IsFalse in
  let rec go a acc =
    match a with
    | And (a,b) -> acc |> go b |> go a
    | True -> acc
    | False -> raise IsFalse
    | a ->
    begin match a, acc with
    (* SJS: common in n-ary branches *)
    | Neg (Test (f, v)), Test (f', v')::_ when f = f' && v<>v' ->
      acc
    | _ -> a::acc
    end
  in
  try go a [] with IsFalse -> [False]

let disjuncts a =
  let exception IsTrue in
  let rec go a acc =
    match a with
    | Or (a,b) -> acc |> go b |> go a
    | True -> raise IsTrue
    | False -> acc
    | a -> a::acc
  in
  try go a [] with IsTrue -> [True]

let nr_of_loops p =
  let rec do_pol p acc =
    match p with
    | Filter _ | Modify _ ->
      acc
    | Seq(p,q) | Ite(_,p,q) ->
      do_pol p (do_pol q acc)
    | While (_,body) | ObserveUpon (body,_)  ->
      do_pol body (acc + 1)
    | Let { body; _ } (* | Repeat (_,body) *) ->
      do_pol body acc
    | Choice choices ->
      List.fold choices ~init:acc ~f:(fun acc (p,_) -> do_pol p acc)
    | Branch branches ->
      List.fold branches ~init:acc ~f:(fun acc (_,p) -> do_pol p acc)
  in
  do_pol p 0

let pp_hv op fmt hv =
  fprintf fmt "@[%s%s%d@]" (fst hv) op (snd hv)

let pp_policy fmt (p : string policy) =
  let rec do_pol ctxt fmt (p : string policy) =
    match p with
    | Filter pred -> do_pred ctxt fmt pred
    | Modify hv -> pp_hv "<-" fmt hv
    | Seq (p1, p2) ->
      begin match ctxt with
        | `PAREN
        | `SEQ_L
        | `SEQ_R -> fprintf fmt "@[%a;@ %a@]" (do_pol `SEQ_L) p1 (do_pol `SEQ_R) p2
        | _ -> fprintf fmt "@[(@[%a;@ %a@])@]" (do_pol `SEQ_L) p1 (do_pol `SEQ_R) p2
      end
    | While (a,p) ->
      fprintf fmt "@[WHILE@ @[<2>%a@]@ DO@ @[<2>%a@]@]"
        (do_pred `COND) a (do_pol `While) p
(*     | Repeat (n,p) ->
      fprintf fmt "@[REPEAT@ @[<2>%d@]@ TIMES@ @[<2>%a@]@]"
        n (do_pol `While) p *)
    | Ite (a, p, q) ->
      fprintf fmt "@[IF@ @[<2>%a@]@ THEN@ @[<2>%a@]@ ELSE@ @[<2>%a@]@]"
        (do_pred `COND) a (do_pol `ITE_L) p (do_pol `ITE_R) q
    | Let { id; init; mut; body } ->
      fprintf fmt "@[@[%a@]@ IN@ @[<0>%a@]"
        do_binding (id, init, mut) (do_pol `PAREN) body
    | Choice ps ->
      fprintf fmt "@[?{@;<1-2>";
      List.iter ps ~f:(fun (p,q) ->
        fprintf fmt "@[%a@ %@@ %a;@;@]" (do_pol `CHOICE) p Prob.pp q);
      fprintf fmt "@;<1-0>}@]"
    | Branch ((a,p)::branches) ->
      fprintf fmt "@[IF@ @[<2>%a@]@ THEN@ @[<2>%a@]@ ELSE@ @[<2>%a@]@]"
        (do_pred `COND) a (do_pol `ITE_L) p (do_pol `ITE_R) (Branch branches)
    | Branch [] ->
      do_pred ctxt fmt False
    | ObserveUpon (p, a) ->
      fprintf fmt "@[DO@ @[<2>%a@]@ THEN OBSERVE @ @[<2>%a@]@]"
        (do_pol `While) p (do_pred `COND) a
  and do_pred ctxt fmt (p : string pred) =
    match p with
    | True -> fprintf fmt "@[1@]"
    | False -> fprintf fmt "@[0@]"
    | Test hv -> pp_hv "=" fmt hv
    | Neg p -> fprintf fmt "@[¬%a@]" (do_pred `Neg) p
    | Or (a1, a2) ->
      begin match ctxt with
        | `PAREN
        | `Or -> fprintf fmt "@[%a@ or@ %a@]" (do_pred `Or) a1 (do_pred `Or) a2
        | _ -> fprintf fmt "@[(@[%a@ or@ %a@])@]" (do_pred `Or) a1 (do_pred `Or) a2
      end
    | And (p1, p2) ->
      begin match ctxt with
        | `PAREN
        | `SEQ_L
        | `SEQ_R -> fprintf fmt "@[%a;@ %a@]" (do_pred `SEQ_L) p1 (do_pred `SEQ_R) p2
        | _ -> fprintf fmt "@[(@[%a;@ %a@])@]" (do_pred `SEQ_L) p1 (do_pred `SEQ_R) p2
      end
  and do_binding fmt (id, init, mut) =
    fprintf fmt "%s@ %s@ :=@ %s"
      (if mut then "var" else "let")
      id
      (match init with
        | Alias f -> f
        | Const v -> Int.to_string v)

  in
  do_pol `PAREN fmt p


(** constructors *)
module Constructors = struct
  let drop = Filter False
  let skip = Filter True
  let test hv = Test hv
  let filter a = Filter a
  let modify hv = Modify hv
  let observe p a = ObserveUpon (p,a)
  (* let repeat n p = Repeat (n,p) *)

  let neg = function
    | True -> False
    | False -> True
    | Neg a -> a
    | a -> Neg a

  let disj a b = match a,b with
    | True, _
    | _, True -> True
    | False, c
    | c, False -> c
    | _ -> Or (a, b)

  let mk_big_disj =
    List.fold ~init:False ~f:disj

  let conj a b = match a,b with
    | False, _
    | _, False -> False
    | True, c
    | c, True -> c
    | _ -> And (a,b)

  let seq p q = match p,q with
    | Filter False, _
    | _, Filter False -> Filter False
    | Filter True, c
    | c, Filter True -> c
    | Filter a, Filter b -> Filter (And (a,b))
    | _ -> Seq (p, q)

  let choice ps =
    (* smash equal -> requires hashconsing *)
    match List.filter ps ~f:(fun (p,r) -> not Prob.(equal r zero)) with
    | [(p,r)] -> assert Prob.(equal r one); p
    | ps -> Choice ps

  let ite a p q = match a with
    | True -> p
    | False -> q
    | _ -> Ite (a, p, q)

  let branch branches =
    List.filter branches ~f:(function
      | (False, _) -> false
      | (_, Filter False) -> false
      | _ -> true)
    |> function
      | [] -> drop
      | [(True, p)] -> p
      | branches -> Branch branches

  let ite_cascade ?(disjoint=true) (xs : 'a list) ~(otherwise: 'field policy)
    ~(f : 'a -> 'field pred * 'field policy) : 'field policy =
    match disjoint with
    | true ->
      List.map xs ~f
      |> branch
    | false ->
      List.fold_right xs ~init:otherwise ~f:(fun x acc ->
        let guard, body = f x in
        ite guard body acc
      )

  let whl a p = match a with
    | True -> drop
    | False -> skip
    | _ -> While (a,p)

  let rec optimize = function
    | Filter a -> filter (optimize_pred a)
    | Modify hv -> modify hv
    | Seq (p, q) -> seq (optimize p) (optimize q)
    | Ite (a, p, q) -> ite (optimize_pred a) (optimize p) (optimize q)
    | Branch branches ->
      List.map branches ~f:(fun (a,p) -> (optimize_pred a, optimize p))
      |> branch
    | While (a, p) -> whl (optimize_pred a) (optimize p)
    | Choice ps -> choice (Util.map_fst ps ~f:optimize)
    | Let { id; init; mut; body } -> Let { id; init; mut; body = optimize body }
    | ObserveUpon (p, a) -> observe (optimize p) (optimize_pred a)
  and optimize_pred a =
    match a with
    | True | False | Test _ -> a
    | Neg a -> neg (optimize_pred a)
    | And (a, b) -> conj (optimize_pred a) (optimize_pred b)
    | Or (a, b) -> disj (optimize_pred a) (optimize_pred b)

(*     let bounded_whl a p ~bound = match a with
    | True -> drop
    | False -> skip
    | _ -> repeat bound (ite a p skip) *)

  let do_whl a p =
    seq p (whl a p)

  let conji n ~f =
    Array.init n ~f
    |> Array.fold ~init:True ~f:conj

  let disji n ~f =
    Array.init n ~f
    |> Array.fold ~init:False ~f:disj

  let seqi n ~f =
    Array.init n ~f
    |> Array.fold ~init:skip ~f:seq

  let mk_big_seq pols =
    List.fold pols ~init:skip ~f:seq

  let then_observe a p =
    ObserveUpon (p, a)

  let choicei n ~f =
    Array.init n ~f
    |> Array.to_list
    |> choice

  let uniformi n ~f =
    choicei n ~f:(fun i -> (f i, Prob.(1//n)))

  let uniform ps =
    let ps = Array.of_list ps in
    let n = Array.length ps in
    uniformi n ~f:(fun i -> ps.(i))


  let mk_big_ite ~default = List.fold ~init:default ~f:(fun q (a, p) -> ite a p q)

  let alias (id, aliasee) ~(mut:bool) body =
    Let { id; init = Alias aliasee; mut; body }
  let local (id, value) ~(mut:bool) body =
    Let { id; init = Const value; mut; body }

  let locals binds body =
    List.fold_right binds ~init:body ~f:(fun (id, value, mut) body ->
      local (id,value) ~mut body
    )

end

module PNK = struct
  include Constructors
  let ( ?? ) hv = filter (test hv)
  let ( ??? ) hv = test hv
  let ( !! ) hv = modify hv
  let ( >> ) p q = seq p q
  let ( & ) a b = conj a b
  let ( ?@ ) dist = choice dist (* ?@[p , 1//2; q , 1//2] *)
  let ( // ) m n = Prob.(m // n)
  let ( @ ) p r = (p,r)
end


(** {2} useful auxilliary functions  *)

(* turn predicate into modification *)
let rec positive_pred_to_mod pred =
  let open PNK in
  match pred with
  | True -> skip
  | False -> drop
  | Test (f,v) -> Modify (f,v)
  | And (p, q) -> positive_pred_to_mod p >> positive_pred_to_mod q
  | Or _ | Neg _ -> failwith "not a positive predicate!"


(* map a policy bottom up *)
let map_pol
  ?(do_pred=fun a -> a)
  ?(filter=fun pred -> Filter pred)
  ?(modify=fun hv -> Modify hv)
  ?(seq=fun p q -> Seq (p,q))
  ?(ite=fun a p q -> Ite (a,p,q))
  ?(branch=fun ps -> Branch ps)
  ?(whl=fun a p -> While (a,p))
  ?(choice=fun ps -> Choice ps)
  ?(letbind=fun id init mut body -> Let { id; init; mut; body})
  ?(obs=fun p a -> ObserveUpon (p,a))
  pol
  =
  let rec do_pol pol =
    match pol with
    | Filter a -> filter (do_pred a)
    | Modify hv -> modify hv
    | Seq (p,q) -> seq (do_pol p) (do_pol q)
    | Ite (a,p,q) -> ite (do_pred a) (do_pol p) (do_pol q)
    | Branch ps -> branch (List.map ps ~f:(fun (a,p) -> (do_pred a, do_pol p)))
    | While (a,p) -> whl (do_pred a) (do_pol p)
    | Choice ps -> choice (Util.map_fst ps ~f:do_pol)
    | Let { id; init; mut; body} -> letbind id init mut (do_pol body)
    | ObserveUpon (p,a) -> obs (do_pol p) (do_pred a)
  in
  do_pol pol


let branches (p : 'field policy) : ('field pred * 'field policy) list =
  let rec go p guard acc =
    match p with
    | Ite (a, p, q) ->
      acc |> go q PNK.(guard & neg a) |> go p PNK.(guard & a)
    | p -> if guard = False then acc else (guard, p)::acc
  in
  go p True []

