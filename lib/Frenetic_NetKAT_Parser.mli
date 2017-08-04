(** parsing NetKAT polcies and predicates  *)

val pol_of_string : ?pos: Lexing.position -> string -> Frenetic_NetKAT.policy
val pred_of_string : ?pos: Lexing.position -> string -> Frenetic_NetKAT.pred
val equalities_of_string : ?pos: Lexing.position -> string 
  -> (Frenetic_NetKAT.policy * Frenetic_NetKAT.policy) list

val pol_of_file : string -> Frenetic_NetKAT.policy
val pred_of_file : string -> Frenetic_NetKAT.pred
val equalities_of_file : string -> (Frenetic_NetKAT.policy * Frenetic_NetKAT.policy) list
