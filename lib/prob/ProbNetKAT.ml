open Core

let fprintf = Format.fprintf

type field = string [@@deriving sexp, show, compare, eq]
module Field = String

type value = int [@@deriving sexp, show, compare, eq]
module Value = Int

type headerval = field * value [@@deriving sexp, compare, eq]

type policy =
  | Skip
  | Drop
  | TestEq of headerval
  | TestNeq of headerval
  | Modify of headerval
  | Seq of policy * policy
  | Ite of policy * policy * policy
  | While of policy * policy
  | Choice of (policy * Prob.t) list
  [@@deriving sexp, compare, eq]

let pp_hv op fmt hv =
  fprintf fmt "@[%s%s%d@]" (fst hv) op (snd hv)

let pp_policy fmt p =
  let rec pol ctxt fmt p =
    match p with
    | Skip -> fprintf fmt "@[0@]"
    | Drop -> fprintf fmt "@[1@]"
    | TestEq hv -> pp_hv "=" fmt hv
    | TestNeq hv -> pp_hv "!=" fmt hv
    | Modify hv -> pp_hv "<-" fmt hv
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
    match pol with
    | Skip 
    | Drop ->
      d
    | TestEq (f,n)
    | TestNeq (f,n)
    | Modify (f,n) ->
      Field.Map.update d f ~f:(function 
        | None -> Value.Set.singleton n 
        | Some ns -> Value.Set.add ns n)
    | Seq (p,q) | While (p,q) ->
      d |> domain p |> domain q
    | Ite (p,q,r) ->
      d |> domain p |> domain q |> domain r
    | Choice ps ->
      List.fold ps ~init:d ~f:(fun d (p,_) -> domain p d)
  in
  domain pol Field.Map.empty

module Syntax = struct
  let ( ?? ) hv = TestEq hv
  let ( !! ) hv = Modify hv
  let ( >> ) p q = Seq (p, q)
  let ( ?@ ) dist = Choice dist (* ?@[p , 1//2; q , 1//2] *)
  let ( // ) m n = Q.(m // n)
end


module Convenience = struct
  let seqi n ~f =
    Array.init n ~f
    |> Array.fold ~init:Drop ~f:(fun p q -> if p = Drop then q else Seq(p, q))

  let choicei n ~f =
    Array.init n ~f
    |> (fun a -> Choice (Array.to_list a))
end
