(** Thin wrapper around Menhir-generated parser, providing a saner interface. *)

module Lexer = Frenetic_NetKAT_Portless_Lexer
module Parser = Frenetic_NetKAT_Portless_Generated_Parser

let pol_of_string ?pos (s : string) =
  Lexer.parse_string ?pos s Parser.pol_eof

let pred_of_string ?pos (s : string) =
  Lexer.parse_string ?pos s Parser.pred_eof

let pol_of_file (file : string) =
  Lexer.parse_file ~file Parser.pol_eof

let pred_of_file (file : string) =
  Lexer.parse_file ~file Parser.pred_eof