(* FIXME: while ppx_import is not compatible with jbuilder, simply copy and paste
   token type here as a workaround. *)
(* type token = [@import: Tokens.token] [@@deriving show] *)
#include "Tokens.ml"
[@@deriving show]

type ('token, 'a) parser =
  (Lexing.lexbuf -> 'token) -> Lexing.lexbuf -> 'a

val parse : Frenetic_netkat.LexBuffer.t
  -> (token,'a) parser -> 'a

val parse_string : ?pos:Lexing.position -> string
  -> (token,'a) parser -> 'a

val parse_file : file:string
  -> (token,'a) parser -> 'a
