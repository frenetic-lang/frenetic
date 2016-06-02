open Core.Std
open Sexplib.Conv
open Frenetic_Decide_Util

module FieldMap = Map.Make(Field)

type packet = Value.t FieldMap.t [@@deriving sexp, compare]
type point = packet * packet [@@deriving sexp, compare]

module PacketSet = Set.Make (struct
  type t = packet [@@deriving sexp, compare]
end)

let packet_to_string pkt = Printf.sprintf "[%s]"
    (String.concat ~sep:";"
       (FieldMap.fold pkt ~init:[]
          ~f:(fun ~key ~data acc -> (Printf.sprintf "%s := %s" (Field.to_string key) (Value.to_string data) :: acc))))

let point_to_string (pkt1, pkt2) = Printf.sprintf "(%s,%s)" (packet_to_string pkt1) (packet_to_string pkt2)

module rec TermBase : sig
  type t = term Hashcons.hash_consed and
  term =
    | Assg of Field.t * Value.t
    | Test of Field.t * Value.t
    | Dup
    | Plus of TermSetBase.t
    | Times of t list
    | Not of t
    | Star of t
    | Zero
    | One

  type int_term =
    | IAssg of Field.t * Value.t
    | ITest of Field.t * Value.t
    | IDup
    | IPlus of Int.Set.t
    | ITimes of int list
    | INot of int
    | IStar of int
    | IZero
    | IOne [@@deriving compare]

  val project: term -> (t -> int) -> int_term
  val comparable_of_term: term -> int_term
  val hashable_of_term: term -> int_term

  val compare: t -> t -> int
  val compare_term: term -> term -> int
  val sexp_of_t: t -> Sexplib.Sexp.t
  val t_of_sexp: Sexplib.Sexp.t -> t
  val sexp_of_term: term -> Sexplib.Sexp.t
  val term_of_sexp: Sexplib.Sexp.t -> term

end = struct
  type t = term Hashcons.hash_consed and
  term =
    | Assg of Field.t * Value.t
    | Test of Field.t * Value.t
    | Dup
    | Plus of TermSetBase.t
    | Times of t list
    | Not of t
    | Star of t
    | Zero
    | One

  type int_term =
    | IAssg of Field.t * Value.t
    | ITest of Field.t * Value.t
    | IDup
    | IPlus of Int.Set.t
    | ITimes of int list
    | INot of int
    | IStar of int
    | IZero
    | IOne [@@deriving compare]

  let project t f =
    match t with
    | Assg (f, v) -> IAssg (f, v)
    | Test (f, v) -> ITest (f, v)
    | Dup -> IDup
    | Plus ts -> IPlus (Int.Set.map ts ~f)
    | Times ts -> ITimes (List.map ts ~f)
    | Not t -> INot (f t)
    | Star t -> IStar (f t)
    | Zero -> IZero
    | One -> IOne
  let comparable_of_term term = project term (fun t -> t.Hashcons.tag)
  let hashable_of_term term = project term (fun t -> t.Hashcons.hkey)

  let compare_term a b =
    let a_comp = (comparable_of_term a) in
    let b_comp = (comparable_of_term b) in
    compare_int_term a_comp b_comp

  let compare a b = compare_term a.Hashcons.node b.Hashcons.node

  let sexp_of_t t = failwith "TODO"
  let t_of_sexp s = failwith "TODO"
  let sexp_of_term t = failwith "TODO"
  let term_of_sexp s = failwith "TODO"

end
and TermSetBase : sig
  include Set.S with type Elt.t = TermBase.t
end = Set.Make (struct
  type t = TermBase.t
  let compare = TermBase.compare
  let t_of_sexp = TermBase.t_of_sexp
  let sexp_of_t = TermBase.sexp_of_t
end)

module Term = struct
  include TermBase

  let rec eval (t : TermBase.t) (pkt : packet) = match t.Hashcons.node with
    | Assg (f,v) -> PacketSet.singleton (FieldMap.add pkt ~key:f ~data:v)
    | Test (f,v) -> begin match FieldMap.find pkt f with
        | Some v' -> if v' = v
          then PacketSet.singleton pkt
          else PacketSet.empty
        | None -> PacketSet.empty
      end
    | Dup -> raise (Failure "t must be dup-free")
    | Plus ts -> TermSetBase.fold ts ~f:(fun acc t -> PacketSet.union (eval t pkt) acc) ~init:PacketSet.empty
    | Times ts -> List.fold ts ~init:(PacketSet.singleton pkt) ~f:(fun accum t ->
        PacketSet.fold accum ~init:PacketSet.empty ~f:(fun acc pkt -> PacketSet.union acc (eval t pkt)))
    | Not t -> let ret = eval t pkt in
      begin
        match PacketSet.length ret with
        | 0 -> PacketSet.singleton pkt
        | 1 -> PacketSet.empty
        | _ -> raise (Failure "Negation of a non-predicate")
      end
    (* TODO: Copy fixpoint code from Frenetic *)
    | Star t -> raise (Failure "NYI")
    | Zero -> PacketSet.empty
    | One -> PacketSet.singleton pkt

  let compare_ab t point =
    let input,output = point in
    PacketSet.exists (eval t input) ~f:(FieldMap.equal Value.equal output)

  let rec to_string (t : t) : string =
    let out_precedence (t : t) : int =
      match t.Hashcons.node with
        | Plus _ -> 0
        | Times _ -> 1
        | Not _ -> 2
        | Star _ -> 3
        | _ -> 4 in
    let protect (u:t) : string =
      let s = to_string u in
      if out_precedence t <= out_precedence u then s
      else Printf.sprintf "(%s)" s in
    let assoc_to_string (op : string) (init : string) (s : string list) : string =
      match s with
        | [] -> init
        | _ -> String.concat ~sep:op s in
    match t.Hashcons.node with
      | Assg (f,v) ->
        Printf.sprintf "%s:=%s"
          (Field.to_string f) (Value.to_string v)
      | Test (f,v) ->
        Printf.sprintf "%s=%s"
          (Field.to_string f) (Value.to_string v)
      | Dup ->
        "dup"
      | Plus (ts) ->
        assoc_to_string " + " "drop"
          (List.map ~f:protect (TermSetBase.elements ts))
      | Times (ts) ->
        assoc_to_string ";" "id" (List.map ~f:protect ts)
      | Not (t) ->
        "~" ^ (protect t)
      | Star (t) ->
        (protect t) ^ "*"
      | Zero ->
        "drop"
      | One ->
        "id"

  (* module H = Make(struct *)
      (* type t = TermBase.term [@@deriving sexp, compare] *)
      (* let equal a b = compare a b = 0 *)
      (* let hash = Hashtbl.hash *)
    (* end) *)

  module H = Hashcons.Make(struct
      type t = TermBase.term

      let equal a b =
        match a, b with
        | Assg (f1, v1), Assg (f2, v2) -> Field.equal f1 f2 && Value.equal v1 v2
        | Test (f1, v1), Test (f2, v2) -> Field.equal f1 f2 && Value.equal v1 v2
        | Dup, Dup -> true
        | Plus t1, Plus t2 -> TermSetBase.equal t1 t2
        | Times t1, Times t2 ->
            List.length t1 = List.length t2 && List.for_all2_exn t1 t2 phys_equal
        | Not t1, Not t2 -> phys_equal t1 t2
        | Star t1, Star t2 -> phys_equal t1 t2
        | Zero, Zero -> true
        | One, One -> true
        | _ -> false

      let hash t = Hashtbl.hash (hashable_of_term t)
    end)

  let hashtbl = H.create 100
  let assg f v = H.hashcons hashtbl (Assg (f,v))
  let test f v = H.hashcons hashtbl (Test (f,v))
  let dup = H.hashcons hashtbl Dup
  let plus ts = H.hashcons hashtbl (Plus ts)
  let times ts = H.hashcons hashtbl (Times (List.fold_right ts ~init:[] ~f:(fun x acc -> match x.Hashcons.node with
      | One -> acc
      | Times ts' -> ts' @ acc
      | _ -> x :: acc)))
  let not t = H.hashcons hashtbl (Not t)
  let star t = H.hashcons hashtbl (Star t)
  let zero = H.hashcons hashtbl Zero
  let one = H.hashcons hashtbl One

  module UnivMap = SetMapF(Field)(Value)
  (* Collect the possible values of each variable *)
  let values (t : TermBase.t) : UnivMap.t =
    let rec collect (m : UnivMap.t) (t : TermBase.t) : UnivMap.t =
      match t.Hashcons.node with
	| (Assg (x,v) | Test (x,v)) -> UnivMap.add x v m
	| Plus s -> TermSetBase.fold s ~init:m ~f:collect
	| Times s -> List.fold_right s ~init:m ~f:(fun a b -> collect b a)
	| (Not x | Star x) -> collect m x
	| (Dup  | Zero  | One ) -> m in
    collect UnivMap.empty t

  let equal t1 t2 = compare t1 t2 = 0
  let rec size t =
    match t.Hashcons.node with
      | Assg(f,v) ->
        1
      | Test(f,v) ->
        1
      | Dup ->
        1
      | Plus ts ->
        TermSetBase.fold ts
          ~f:(fun n ti -> (size ti) + n)
          ~init:1
      | Times ts ->
        List.fold_left ts
          ~f:(fun n ti -> n + (size ti))
          ~init:1
      | Not t -> 1 + size t
      | Star t -> 1 + size t
      | Zero -> 1
      | One -> 1
end

module TermSet = struct
  include TermSetBase
  let to_string ts = Printf.sprintf "{%s}" (String.concat ~sep:", " (List.map (elements ts) Term.to_string))
end

module Formula = struct
  type t =
    | Eq of Term.t * Term.t
    | Le of Term.t * Term.t

  let to_string (f:t) : string =
    match f with
      | Eq (s,t) ->
        Printf.sprintf "%s == %s"
          (Term.to_string s) (Term.to_string t)
      | Le (s,t) ->
        Printf.sprintf "%s <= %s"
          (Term.to_string s) (Term.to_string t)

  let compare (f1:t) (f2:t) : int =
    match f1,f2 with
      | Eq(s1,t1), Eq(s2,t2) ->
        let cmp = Term.compare s1 s2 in
        if cmp <> 0 then cmp
        else Term.compare t1 t2
      | Le(s1,t1), Le(s2,t2) ->
        let cmp = Term.compare s1 s2 in
        if cmp <> 0 then cmp
        else Term.compare t1 t2
      | Eq _, _ -> -1
      | _ -> 1

  let equal (f1:t) (f2:t) : bool =
    compare f1 f2 = 0

  let terms (f: t) =
    match f with
      | Eq (s,t)
      | Le (s,t) -> (s,t)
end
