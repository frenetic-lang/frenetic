(* FIXME: while ppx_import is not compatible with jbuilder, simply copy and paste
   token type here as a workaround. *)
(* type token = [@import: Tokens.token] [@@deriving show] *)
#include "Tokens.ml"
[@@deriving show]

type ('token, 'a) parser =
  (Lexing.lexbuf -> 'token) -> Lexing.lexbuf -> 'a

val parse : ?ppx:bool -> LexBuffer.t
  -> (token,'a) parser -> 'a

val parse_string : ?ppx:bool -> ?pos:Lexing.position -> string
  -> (token,'a) parser -> 'a

val parse_file : ?ppx:bool -> file:string
  -> (token,'a) parser -> 'a
