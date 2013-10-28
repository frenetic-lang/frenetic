type policy = NetKAT_Types.policy
type lf_policy
type link

val lf_policy_to_policy : lf_policy -> policy
val link_to_policy : link -> policy

type 'a aregex =
  | Char of 'a
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

val regex_to_switch_lf_policies : regex -> (lf_policy SwitchMap.t * link list)
val switch_policies_to_policy : policy SwitchMap.t -> policy
val dehopify : policy -> (policy SwitchMap.t * link list)
