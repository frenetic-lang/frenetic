type policy = NetKAT_Types.policy
type lf_policy
type link

type regex =
  | Char of lf_policy * link
  | Alt of regex * regex
  | Cat of regex * regex
  | Kleene of regex
  | Empty


val regex_of_policy : policy -> regex
val regex_to_policy : regex -> policy
