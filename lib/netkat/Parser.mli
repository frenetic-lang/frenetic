(** parsing NetKAT polcies and predicates  *)

val pol_of_string : ?pos: Lexing.position -> string -> Syntax.policy
val pred_of_string : ?pos: Lexing.position -> string -> Syntax.pred

val pol_of_file : string -> Syntax.policy
val pred_of_file : string -> Syntax.pred

module Portless : sig
  val pol_of_string : ?pos: Lexing.position -> string -> Syntax.policy
  val pred_of_string : ?pos: Lexing.position -> string -> Syntax.pred

  val pol_of_file : string -> Syntax.policy
  val pred_of_file : string -> Syntax.pred
end