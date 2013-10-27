type policy = NetKAT_Types.policy
type lf_policy
type link

val lf_policy_to_policy : lf_policy -> policy
val link_to_policy : link -> policy

type regex =
  | Char of lf_policy * link
  | Alt of regex * regex
  | Cat of regex * regex
  | Kleene of regex
  | Empty


val regex_of_policy : policy -> regex
val regex_to_policy : regex -> policy

val regex_to_string : regex -> string
val lf_policy_to_string : lf_policy -> string
