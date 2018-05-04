(** Thin wrapper around Menhir-generated parser, providing a saner interface. *)

val pol_of_string : ?pos: Lexing.position -> string -> string Ast.pol
val formula_of_string : ?pos: Lexing.position -> string -> string Ast.formula

val pol_of_file : string -> string Ast.pol
val formula_of_file : string -> string Ast.formula
