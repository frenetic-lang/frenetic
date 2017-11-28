(** Thin wrapper around Menhir-generated parser, providing a saner interface. *)

module MAKE(Lexer : module type of Lexer)
    (Parser : module type of Generated_Parser) = struct

  let pol_of_string ?pos (s : string) =
    Lexer.parse_string ?pos s Parser.pol_eof

  let pred_of_string ?pos (s : string) =
    Lexer.parse_string ?pos s Parser.pred_eof

  let pol_of_file (file : string) =
    Lexer.parse_file ~file Parser.pol_eof

  let pred_of_file (file : string) =
    Lexer.parse_file ~file Parser.pred_eof

end

include MAKE(Lexer)(Generated_Parser)

(** portless extensions *)
module Portless = MAKE(Lexer)(Portless_Generated_Parser)