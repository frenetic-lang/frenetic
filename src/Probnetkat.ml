open Core

[@@@ocaml.warning "-30"]

let fprintf = Format.fprintf


(** {2} fields and values *)
type field = string [@@deriving sexp, show, compare, eq, hash]
module Field = String

type value = int [@@deriving sexp, show, compare, eq, hash]
module Value = Int

type header_val = field * value [@@deriving sexp, compare, eq, hash]


(** {2} predicates and policies; open types, in case we want to add hash-consing later *)

(* we may want to add local fields *)
type meta_init =
  | Alias of header_val
  | Const of int64
  [@@deriving sexp]

type 'pred pred0 =
  | True
  | False
  | Test of header_val
  | And of 'pred * 'pred
  | Or of 'pred * 'pred
  | Neg of 'pred
  [@@deriving sexp, compare, hash]

and ('pol, 'pred) policy0 =
  | Filter of 'pred
  | Modify of header_val
  | Ite of 'pred * 'pol * 'pol
  | While of 'pred * 'pol
  | Choice of ('pol * Prob.t) list
  [@@deriving sexp, compare, hash]

type pred = { kind: pred pred0 }
  [@@deriving sexp, compare, hash]
and policy = { kind: (policy, pred) policy0 }
  [@@deriving sexp, compare, hash]

let pp_hv op fmt hv =
  fprintf fmt "@[%s%s%d@]" (fst hv) op (snd hv)

let pp_policy0 fmt (p : policy) =
  let rec do_pol ctxt fmt (p : policy) = do_pol0 ctxt fmt p.kind
  and do_pol0 ctxt fmt (p : ('a, 'b) policy0) =
    match p with
    | Filter pred -> do_pred ctxt fmt pred
    | Modify hv -> pp_hv "<-" fmt hv
    | While (a,p) ->
      fprintf fmt "@[WHILE@ @[<2>%a@]@ DO@ @[<2>%a@]@]"
        (do_pred `COND) a (do_pol `While) p
    | Ite (a, p, q) ->
      fprintf fmt "@[IF@ @[<2>%a@]@ THEN@ @[<2>%a@]@ ELSE@ @[<2>%a@]@]"
        (do_pred `COND) a (do_pol `ITE_L) p (do_pol `ITE_R) q
    | Choice ps ->
      fprintf fmt "@[?{@;<1-2>";
      List.iter ps ~f:(fun (p,q) ->
        fprintf fmt "@[%a@ %@@ %a;@;@]" (do_pol `CHOICE) p Q.pp_print q);
      fprintf fmt "@;<1-0>}@]"
  and do_pred ctxt fmt (p : pred) = do_pred0 ctxt fmt p.kind
  and do_pred0 ctxt fmt p =
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
  in
  do_pol `PAREN fmt p

(*

type domain = Value.Set.t Field.Map.t

module type Domain = sig
  val domain : domain
end

*)

(*

(* compute domain of each field *)
let domain pol : domain =
  let rec domain pol d =
    match pol.p with
    | Skip
    | Drop ->
      d
    | Test (f,n)
    | Modify (f,n) ->
      Field.Map.update d f ~f:(function
        | None -> Value.Set.singleton n
        | Some ns -> Value.Set.add ns n)
    | Neg p -> domain p d
    | Or(p,q) | Seq (p,q) | While (p,q) ->
      d |> domain p |> domain q
    | Ite (p,q,r) ->
      d |> domain p |> domain q |> domain r
    | Choice ps ->
      List.fold ps ~init:d ~f:(fun d (p,_) -> domain p d)
  in
  domain pol Field.Map.empty

(** constructors *)
module Constructors = struct
  module Dumb = struct
    let drop = { p = Drop; pred = true; determ = true }
    let skip = { p = Skip; pred = true; determ = true }
    let test hv = { p = Test hv; pred = true; determ = true }
    let modify hv = { p = Modify hv; pred = true; determ = true }
    let neg a =
      assert (a.determ && a.pred);
      { a with p = Neg a }
    let disj a b =
      assert (a.determ && a.pred);
      assert (b.determ && b.pred);
      { a with p = Or (a,b) }
    let seq p q = { p = Seq(p,q); pred = p.pred && q.pred; determ = p.determ && q.determ }
    let choice ps =
      assert (List.fold ~init:Q.zero ~f:Q.(+) (List.map ps ~f:snd) = Q.one);
      { p = Choice ps; pred = false; determ = false }
    let ite a p q =
      assert (a.determ && a.pred);
      { p = Ite (a,p,q); pred = p.pred && q.pred; determ = p.determ && q.determ }
    let mk_while a p =
      assert (a.determ && a.pred);
      { p with p = While(a,p) }

    let seqi n ~f =
      Array.init n ~f
      |> Array.fold ~init:skip ~f:seq

    let choicei n ~f =
      Array.init n ~f
      |> Array.to_list
      |> choice

    let mk_union = disj
    let mk_big_union ~init = List.fold ~init ~f:(fun p q -> if p = drop then q
                                                  else mk_union p q)
    let mk_big_ite ~default = List.fold ~init:default ~f:(fun q (a, p) -> ite a p q)

  end

  module Smart = struct
    let drop = Dumb.drop
    let skip = Dumb.skip
    let test = Dumb.test
    let modify = Dumb.modify

    let neg a =
      match a.p with
      | Neg { p } -> { a with p }
      | _ -> Dumb.neg a

    let disj a b =
      if a = skip || b = drop then a else
      if b = skip || a = drop then b else
      Dumb.disj a b

    let seq p q =
      (* use physical equality? *)
      if p = drop || q = skip then p else
      if q = drop || p = skip then q else
      Dumb.seq p q

    let choice ps =
      (* smash equal -> requires hashconsing *)
      match List.filter ps ~f:(fun (p,r) -> not Q.(equal r zero)) with
      | [(p,r)] -> assert Q.(equal r one); p
      | ps -> Dumb.choice ps

    let ite a p q =
      if a = drop then q else
      if a = skip then p else
      (* if p = q then p else *)
      Dumb.ite a p q

    let mk_while a p =
      if a = drop then skip else
      Dumb.mk_while a p

    let seqi n ~f =
      Array.init n ~f
      |> Array.fold ~init:skip ~f:seq

    let choicei n ~f =
      Array.init n ~f
      |> Array.to_list
      |> choice

    let mk_union = disj
    let mk_big_union ~init = List.fold ~init ~f:(fun p q -> if p = drop then q
                                                  else mk_union p q)
    let mk_big_ite ~default = List.fold ~init:default ~f:(fun q (a, p) -> ite a p q)

  end
end


module Syntax = struct
  module Dumb = struct
    include Constructors.Dumb
    let ( ?? ) hv = test hv
    let ( !! ) hv = modify hv
    let ( >> ) p q = seq p q
    let ( & ) a b = disj a b
    let ( ?@ ) dist = choice dist (* ?@[p , 1//2; q , 1//2] *)
    let ( // ) m n = Q.(m // n)
    let ( @ ) p r = (p,r)
  end

  module Smart = struct
    include Constructors.Smart
    let ( ?? ) hv = test hv
    let ( !! ) hv = modify hv
    let ( >> ) p q = seq p q
    let ( & ) a b = disj a b
    let ( ?@ ) dist = choice dist (* ?@[p , 1//2; q , 1//2] *)
    let ( // ) m n = Q.(m // n)
    let ( @ ) p r = (p,r)
  end
end
 *)
