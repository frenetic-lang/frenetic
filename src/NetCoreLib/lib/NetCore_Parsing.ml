open NetCore_SurfaceSyntax
exception CompileError of string

let sprintf = Format.sprintf

let string_of_pos pos = 
  let open Lexing in
  sprintf "%s, line %d, column %d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)

let compile_pol f = function
  | Pol p ->
    Pol (f p)
  | PolStream (p_lwt, p_stream) ->
    PolStream (p_lwt, NetCore_Stream.map f p_stream)

let compile_pol2 f = function
  | (Pol p1, Pol p2) -> 
    Pol (f p1 p2)
  | (PolStream (p1_lwt, p1_stream), Pol p2) ->
    PolStream  (p1_lwt, NetCore_Stream.map (fun p1 -> f p1 p2) p1_stream)
  | (Pol p1, PolStream (p2_lwt, p2_stream)) ->
     PolStream (p2_lwt, NetCore_Stream.map (fun p2 -> f p1 p2) p2_stream)
  | (PolStream (p1_lwt, p1_stream), PolStream (p2_lwt, p2_stream)) ->
     (* TODO(arjun): could print source location of the program that died!!! *)
     (* TODO(arjun): blow up if either dies. *)
    PolStream (Lwt.join [p1_lwt; p2_lwt],
               NetCore_Stream.map2 (fun p1 p2 -> f p1 p2) p1_stream p2_stream)

let rec compile (env : env) = function
  | HandleSwitchEvent (pos, f) -> Pol (Pol.HandleSwitchEvent f)
  | Par (pos, e1, e2) ->
    compile_pol2 (fun p1 p2 -> Pol.Union (p1, p2))
      (compile env e1, compile env e2)
  | Seq (pos, e1, e2) ->
    compile_pol2
      (fun p1 p2 -> Pol.Seq (p1, p2))
      (compile env e1, compile env e2)
  | Filter (pos, pred) -> Pol (Pol.Filter pred)
  | Action (pos, act) -> Pol (Pol.Action act)
  | ITE (pos, pred, e1, e2) ->
    compile_pol2
      (fun p1 p2 -> Pol.ITE (pred, p1, p2))
      (compile env e1, compile env e2)
  | Id (pos, x) ->
    begin 
      try Env.find x env
      with Not_found ->
        raise (CompileError 
                 (sprintf "%s: variable %s is not defined"
                    (string_of_pos pos) x))
    end
  | Let (pos, binds, body) -> 
    compile
      (List.fold_left (fun env' (x, e) -> Env.add x (compile env e) env') env binds)
      body
  | Transform (pos, f, e) -> compile_pol f (compile env e)
  | Value v -> v
  | Slice (pos, ingress, e, egress) -> 
    failwith "NYI: slice surface syntax."

let rec compile_top (env : env) = function
  | Bind (pos, x, exp, rest) ->
    compile_top (Env.add x (compile env exp) env) rest
  | Main (pos, exp) -> compile env exp
  | Include _ -> failwith "unexpected include"

let compile_program exp = 
  match compile_top init_env exp with
    | PolStream (lwt_e, stream) -> (lwt_e, stream)
    | Pol pol -> (fst (Lwt.wait ()), NetCore_Stream.constant pol)



let rec splice_top included rest = match included with
  | Include (pos, filename, rest') -> 
    failwith "unexpected include2"
  | Main (pos, Action (_, action)) -> 
    if action = NetCore_Action.Output.drop then
      rest
    else
      raise (CompileError 
               (sprintf "%s: included module has a main-expression  \
                         (only top-level let expressions allowed"
                  (string_of_pos pos)))
  | Main (pos, _) ->
      raise (CompileError 
               (sprintf "%s: included module has a main-expression  \
                         (only top-level let expressions allowed"
                  (string_of_pos pos)))

  | Bind (p, x, e, included) ->
    Bind (p, x, e, splice_top included rest)

and expand_top = function
  | Include (pos, filename, rest) ->
    splice_top (parse_by_extension filename) rest
  | Main (pos, exp) -> Main (pos, exp)
  | Bind (p, x, e, rest) -> Bind (p, x, e, expand_top rest)

and literate_lexer (lexbuf : Lexing.lexbuf) : NetCore_Parser.token =
  let open Lexing in
  let p = lexeme_start_p lexbuf in
  if p.pos_cnum - p.pos_bol = 0 then
    (match !NetCore_Lexer.st with
      | NetCore_Lexer.LiterateBlock -> NetCore_Lexer.token lexbuf
      | _ -> NetCore_Lexer.literate lexbuf)
  else
    NetCore_Lexer.token lexbuf

and parse_from_lexbuf is_literate lexbuf name =
  let open Lexing in
    try
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
      let lexer = if is_literate then
          literate_lexer
        else
          NetCore_Lexer.token in
      expand_top (NetCore_Parser.program lexer lexbuf)
    with
      | Failure "lexing: empty token" ->
        failwith (Printf.sprintf "lexical error at %s %s"
                    (string_of_pos lexbuf.lex_curr_p)
                    (lexeme lexbuf))
      | Parsing.Parse_error ->
        failwith (Printf.sprintf "parse error at %s; unexpected token %s"
                    (string_of_pos lexbuf.lex_curr_p)
                    (lexeme lexbuf))

and parse_from_chan cin name =
  parse_from_lexbuf false (Lexing.from_channel cin) name

and parse_literate_from_chan cin name =
  parse_from_lexbuf true (Lexing.from_channel cin) name

and parse_by_extension filename =
  if String.length filename < 3 then
    failwith "missing file extension"
  else if Str.last_chars filename 3 = ".md" then
    parse_literate_from_chan (open_in filename) filename
  else if Str.last_chars filename 3 = ".nc" then
    parse_from_chan (open_in filename) filename
  else 
    failwith "unknown file extension"
