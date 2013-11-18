open Types
type lf_policy
type link

val lf_policy_to_policy : lf_policy -> policy
val link_to_policy : link -> policy

type 'a aregex =
  | Char of 'a
  | Pick of 'a aregex * 'a aregex
  | Alt of 'a aregex * 'a aregex
  | Cat of 'a aregex * 'a aregex
  | Kleene of 'a aregex
  | Empty

type pchar = lf_policy * link
type regex = pchar aregex

val regex_of_policy : policy -> regex
val regex_to_policy : regex -> policy

val regex_to_string : regex -> string
val lf_policy_to_string : lf_policy -> string

val regex_to_aregex : regex -> int aregex * ((int, pchar) Hashtbl.t)
val regex_of_aregex : int aregex -> (int, pchar) Hashtbl.t -> regex

module SwitchMap : Map.S
  with type key = VInt.t

module LinkSet : Set.S
  with type elt = link

type 'a dehopified = 'a * ('a SwitchMap.t) * LinkSet.t * 'a

val regex_to_switch_policies : regex -> policy dehopified
val dehopify : policy -> policy dehopified
