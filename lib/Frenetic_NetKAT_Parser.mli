(** parsing NetKAT polcies and predicates  *)

val pol_of_string : ?pos: Lexing.position -> string -> Frenetic_NetKAT.policy
val pred_of_string : ?pos: Lexing.position -> string -> Frenetic_NetKAT.pred

val pol_of_file : string -> Frenetic_NetKAT.policy
val pred_of_file : string -> Frenetic_NetKAT.pred
