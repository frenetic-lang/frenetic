open Format

let string_of_position p =
  let open Lexing in
  Format.sprintf "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

let parse_from_chan cin name =
  let open Lexing in
  let lexbuf = Lexing.from_channel cin in
    try
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
      MininetParser.program MininetLexer.token lexbuf
    with
      |  Failure "lexing: empty token" ->
           failwith (sprintf "lexical error at %s"
                       (string_of_position lexbuf.lex_curr_p))
      | MininetParser.Error ->
           failwith (sprintf "parse error at %s; unexpected token %s"
                       (string_of_position lexbuf.lex_curr_p)
                       (lexeme lexbuf))

