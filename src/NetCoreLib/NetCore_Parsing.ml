let string_of_position p =
  let open Lexing in
  Printf.sprintf "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

let literate_lexer (lexbuf : Lexing.lexbuf) : NetCore_Parser.token =
  let open Lexing in
  let p = lexeme_start_p lexbuf in
  if p.pos_cnum - p.pos_bol = 0 then
    (match !NetCore_Lexer.st with
      | NetCore_Lexer.LiterateBlock -> NetCore_Lexer.token lexbuf
      | _ -> NetCore_Lexer.literate lexbuf)
  else
    NetCore_Lexer.token lexbuf

let parse_from_lexbuf is_literate lexbuf name =
  let open Lexing in
    try
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
      let lexer = if is_literate then
          literate_lexer
        else
          NetCore_Lexer.token in
      NetCore_SurfaceSyntax.compile_program
        (NetCore_Parser.program lexer lexbuf)
    with
      | Failure "lexing: empty token" ->
        failwith (Printf.sprintf "lexical error at %s %s"
                    (string_of_position lexbuf.lex_curr_p)
                    (lexeme lexbuf))
      | Parsing.Parse_error ->
        failwith (Printf.sprintf "parse error at %s; unexpected token %s"
                    (string_of_position lexbuf.lex_curr_p)
                    (lexeme lexbuf))

let parse_from_chan cin name =
  parse_from_lexbuf false (Lexing.from_channel cin) name

let parse_literate_from_chan cin name =
  parse_from_lexbuf true (Lexing.from_channel cin) name
