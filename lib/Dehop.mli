open Types

val policy_to_dehopd_policy : policy -> policy * policy * policy * policy
val dehopd_policy_to_policy : policy * policy * policy * policy -> policy

(* The resultant policy will have no link terms in it *)
val dehop_policy : policy -> policy
