let string_of_position p =
  let open Lexing in
  Printf.sprintf "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

let parse_from_lexbuf lexbuf name =
  let open Lexing in
    try
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
      NetCore_SurfaceSyntax.compile_program
        (NetCore_Parser.program NetCore_Lexer.token lexbuf)
    with
      | Failure "lexing: empty token" ->
        failwith (Printf.sprintf "lexical error at %s"
                    (string_of_position lexbuf.lex_curr_p))
      | Parsing.Parse_error ->
        failwith (Printf.sprintf "parse error at %s; unexpected token %s"
                    (string_of_position lexbuf.lex_curr_p)
                    (lexeme lexbuf))

let parse_from_chan cin name =
  parse_from_lexbuf (Lexing.from_channel cin) name
