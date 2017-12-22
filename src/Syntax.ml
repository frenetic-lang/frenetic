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
  | While of 'field pred * 'field policy
  | Choice of ('field policy * Prob.t) list
  | Let of { id : 'field; init : 'field meta_init; mut : bool; body : 'field policy }
  | Repeat of int * 'field policy
  [@@deriving sexp, compare, hash]

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
    | Repeat (n,p) ->
      fprintf fmt "@[REPEAT@ @[<2>%d@]@ TIMES@ @[<2>%a@]@]"
        n (do_pol `While) p
    | Ite (a, p, q) ->
      fprintf fmt "@[IF@ @[<2>%a@]@ THEN@ @[<2>%a@]@ ELSE@ @[<2>%a@]@]"
        (do_pred `COND) a (do_pol `ITE_L) p (do_pol `ITE_R) q
    | Let { id; init; mut; body } ->
      fprintf fmt "@[@[%a@]@ IN@ @[<0>%a@]"
        do_binding (id, init, mut) (do_pol `PAREN) body
    | Choice ps ->
      fprintf fmt "@[?{@;<1-2>";
      List.iter ps ~f:(fun (p,q) ->
        fprintf fmt "@[%a@ %@@ %a;@;@]" (do_pol `CHOICE) p Q.pp_print q);
      fprintf fmt "@;<1-0>}@]"
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
  (* module Dumb = struct *)
    let drop = Filter False
    let skip = Filter True
    let test hv = Test hv
    let filter a = Filter a
    let modify hv = Modify hv
    let repeat n p = Repeat (n,p)

    let neg a = match a with
      | Neg a -> a
      | _ -> Neg a

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
      | _ -> Seq (p, q)

    let choice ps =
      (* smash equal -> requires hashconsing *)
      match List.filter ps ~f:(fun (p,r) -> not Q.(equal r zero)) with
      | [(p,r)] -> assert Q.(equal r one); p
      | ps -> Choice ps

    let ite a p q = match a with
      | True -> p
      | False -> q
      | _ -> Ite (a, p, q)

    let ite_cascade (xs : 'a list) ~(otherwise: 'field policy)
      ~(f : 'a -> 'field pred * 'field policy) : 'field policy =
      List.fold_right xs ~init:otherwise ~f:(fun x acc ->
        let guard, body = f x in
        ite guard body acc
      )

    let whl a p = match a with
      | True -> drop
      | False -> skip
      | _ -> While (a,p)

    let bounded_whl a p ~bound = match a with
      | True -> drop
      | False -> skip
      | _ -> repeat bound (ite a p skip)

    let do_whl a p =
      seq p (whl a p)

    let conji n ~f =
      Array.init n ~f
      |> Array.fold ~init:True ~f:conj

    let seqi n ~f =
      Array.init n ~f
      |> Array.fold ~init:skip ~f:seq

    let mk_big_seq pols =
      List.fold pols ~init:skip ~f:seq

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
  let ( // ) m n = Q.(m // n)
  let ( @ ) p r = (p,r)
end


(** useful auxilliary functions  *)

let rec positive_pred_to_mod pred =
  let open PNK in
  match pred with
  | True -> skip
  | False -> drop
  | Test (f,v) -> Modify (f,v)
  | And (p, q) -> positive_pred_to_mod p >> positive_pred_to_mod q
  | Or _ | Neg _ -> failwith "not a positive predicate!"
