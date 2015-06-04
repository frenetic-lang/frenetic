open OUnitHack
(* open Frenetic_OpenFlow0x01 *)

open QuickCheck

module Gen = QuickCheck_gen

module LatticeTest(L : sig
  type t
  val arbitrary_t : t arbitrary

  val match_all : t

  val less_eq : t -> t -> bool
  val eq : t -> t -> bool
  val join : t -> t -> t

  val string_of : t -> string
end) = struct

  let t_quickCheck prop =
    let test = testable_fun L.arbitrary_t L.string_of testable_bool in
    match quickCheck test prop with
      | Success -> true
      | Failure _ -> failwith "No failure expected"
      | Exhausted _ -> failwith "No exhaustion expected"

  let t2_quickCheck prop =
    let arb =
      let open Gen in
      L.arbitrary_t >>= fun p1 ->
      L.arbitrary_t >>= fun p2 ->
        ret_gen (p1, p2) in
    let show (p1, p2) =
      Printf.sprintf "%s, %s" (L.string_of p1) (L.string_of p2) in
    let test = testable_fun arb show testable_bool in
    match quickCheck test prop with
      | Success -> true
      | Failure _ -> failwith "No failure expected"
      | Exhausted _ -> failwith "No exhaustion expected"

  let t3_quickCheck prop =
    let arb =
      let open Gen in
      L.arbitrary_t >>= fun p1 ->
      L.arbitrary_t >>= fun p2 ->
      L.arbitrary_t >>= fun p3 ->
        ret_gen (p1, p2, p3) in
    let show (p1, p2, p3) =
      Printf.sprintf "%s, %s, %s"
        (L.string_of p1) (L.string_of p2) (L.string_of p3) in
    let test = testable_fun arb show testable_bool in
    match quickCheck test prop with
      | Success -> true
      | Failure _ -> failwith "No failure expected"
      | Exhausted _ -> failwith "No exhaustion expected"

  let implies a b = b || (not a)

  open L

  TEST "eq reflexive: eq p p" =
    let prop_eq_reflexive p =
      eq p p in
    t_quickCheck prop_eq_reflexive

  TEST "eq symmetric: eq p1 p2 <=> eq p2 p1" =
    let prop_eq_symmetric (p1, p2) =
      eq p1 p2 = eq p2 p1 in
    t2_quickCheck prop_eq_symmetric

  TEST "eq transitive: eq p1 p2 && eq p2 p3 => eq p1 p3" =
    let prop_eq_transitive (p1, p2, p3) =
      implies (eq p1 p2 && eq p2 p3) (eq p1 p3) in
    t3_quickCheck prop_eq_transitive

  TEST "less_eq reflexivity: less_eq p p" =
    let prop_reflexive p = less_eq p p = true in
    t_quickCheck prop_reflexive

  TEST "less_eq antisymmetry: less_eq p1 p2 && less_eq p2 p1 <=> p1 = p2" =
    let prop_antisymmetry (p1, p2) =
      (less_eq p1 p2 && less_eq p2 p1) = (eq p1 p2) in
    t2_quickCheck prop_antisymmetry

  TEST "less_eq transitivity: less_eq p1 p2 && less_eq p2 p3 => less_eq p1 p3" =
    let prop_transitivity (p1, p2, p3) =
      implies (less_eq p1 p2 && less_eq p2 p3) (less_eq p2 p3) in
    t3_quickCheck prop_transitivity

  TEST "less_eq top: less_eq p match_all" =
    let prop_top p =
      less_eq p match_all in
    t_quickCheck prop_top

  TEST "join symmetry: join p1 p2 <=> join p2 p1" =
    let prop_symmetry (p1, p2) = eq (join p1 p2) (join p2 p1) in
    t2_quickCheck prop_symmetry

  TEST "join exact: less_eq p1 (join p1 p2) && less_eq p2 (join p1 p2)" =
    let prop_exact (p1, p2) =
      less_eq p1 (join p1 p2) && less_eq p2 (join p1 p2) in
    t2_quickCheck prop_exact

  TEST "join least: less_eq p1 p3 && less_eq p2 p3 <=> less_eq (join p1 p2) p3" =
    let prop_least (p1, p2, p3) =
      (less_eq p1 p3 && less_eq p2 p3) = (less_eq (join p1 p2) p3) in
    t3_quickCheck prop_least

  TEST "join comparable least: less_eq p1 p2 <=> join p1 p2 = p2" =
    (* This is the same as "join least" when p2 = p3 *)
    let prop_comparable_least (p1, p2) =
      (less_eq p1 p2) = (eq (join p1 p2) p2) in
    t2_quickCheck prop_comparable_least
      
  TEST "eq partial: eq p1 p2 <=> less_eq p1 p2 && less_eq p2 p1" =
    let prop_eq_partial (p1, p2) =
      eq p1 p2 = (less_eq p1 p2 && less_eq p2 p1) in
    t2_quickCheck prop_eq_partial

end

module Ip = LatticeTest(struct
  include Frenetic_OpenFlow.Pattern.Ip
  let arbitrary_t = Arbitrary_Frenetic_OpenFlow.arbitrary_ip_mask
end)

module Pattern = LatticeTest(struct
  include Frenetic_OpenFlow.Pattern
  let arbitrary_t = Arbitrary_Frenetic_OpenFlow.arbitrary_pattern
end)

