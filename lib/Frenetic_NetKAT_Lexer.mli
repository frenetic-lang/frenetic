type token = [%import: Frenetic_NetKAT_Tokens.token] [@@deriving show]

type ('token, 'a) parser =
  (Lexing.lexbuf -> 'token) -> Lexing.lexbuf -> 'a

val parse : ?ppx:bool -> LexBuffer.t 
  -> (token,'a) parser -> 'a

val parse_string : ?ppx:bool -> ?pos:Lexing.position -> string 
  -> (token,'a) parser -> 'a
  
val parse_file : ?ppx:bool -> file:string 
  -> (token,'a) parser -> 'a