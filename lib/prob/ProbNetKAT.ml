open Core

let fprintf = Format.fprintf

type field = string [@@deriving sexp, show, compare, eq]
module Field = String

type value = int [@@deriving sexp, show, compare, eq]
module Value = Int

type headerval = field * value [@@deriving sexp, compare, eq]

(** TODO: make private in mli  *)
type policy =
  { p : policy0;
    pred : bool;
    determ : bool;
  }
and policy0 =
  | Skip
  | Drop
  | Test of headerval
  | Modify of headerval
  | Neg of policy
  | Seq of policy * policy
  | Ite of policy * policy * policy
  | While of policy * policy
  | Choice of (policy * Prob.t) list
  [@@deriving sexp, compare, eq]

let pp_hv op fmt hv =
  fprintf fmt "@[%s%s%d@]" (fst hv) op (snd hv)

let pp_policy fmt p =
  let rec pol ctxt fmt p =
    match p.p with
    | Skip -> fprintf fmt "@[1@]"
    | Drop -> fprintf fmt "@[0@]"
    | Test hv -> pp_hv "=" fmt hv
    | Modify hv -> pp_hv "<-" fmt hv
    | Neg p -> fprintf fmt "@[¬%a@]" (pol `Neg) p
    | Seq (p1, p2) ->
      begin match ctxt with
        | `PAREN
        | `SEQ_L
        | `SEQ_R -> fprintf fmt "@[%a;@ %a@]" (pol `SEQ_L) p1 (pol `SEQ_R) p2
        | _ -> fprintf fmt "@[(@[%a;@ %a@])@]" (pol `SEQ_L) p1 (pol `SEQ_R) p2
      end
    | While (a,p) ->
      fprintf fmt "@[WHILE@ @[<2>%a@]@ DO@ @[<2>%a@]@]"
        (pol `COND) a (pol `While) p
    | Ite (a, p, q) ->
      fprintf fmt "@[IF@ @[<2>%a@]@ THEN@ @[<2>%a@]@ ELSE@ @[<2>%a@]@]"
        (pol `COND) a (pol `ITE_L) p (pol `ITE_R) q
    | Choice ps ->
      fprintf fmt "@[?{@;<1-2>";
      List.iter ps ~f:(fun (p,q) ->
        fprintf fmt "@[%a@ %@@ %a;@;@]" (pol `CHOICE) p Q.pp_print q);
      fprintf fmt "@;<1-0>}@]"
  in
  pol `PAREN fmt p

type domain = Value.Set.t Field.Map.t

module type Domain = sig
  val domain : domain
end

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
    | Seq (p,q) | While (p,q) ->
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
    
    let seq p q =
      (* use physical equality? *)
      if p = drop || q = drop then drop else
      if p = skip then q else
      if q = skip then p else
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

  end
end


module Syntax = struct
  module Dumb = struct
    include Constructors.Dumb
    let ( ?? ) hv = test hv
    let ( !! ) hv = modify hv
    let ( >> ) p q = seq p q
    let ( ?@ ) dist = choice dist (* ?@[p , 1//2; q , 1//2] *)
    let ( // ) m n = Q.(m // n)
    let ( @ ) p r = (p,r)
  end

  module Smart = struct
    include Constructors.Smart
    let ( ?? ) hv = test hv
    let ( !! ) hv = modify hv
    let ( >> ) p q = seq p q
    let ( ?@ ) dist = choice dist (* ?@[p , 1//2; q , 1//2] *)
    let ( // ) m n = Q.(m // n)
    let ( @ ) p r = (p,r)
  end
end
