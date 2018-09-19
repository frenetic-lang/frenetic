open Core


(*===========================================================================*)
(* FLAGS                                                                     *)
(*===========================================================================*)

module Flag = struct
  open Command.Spec

  let openflow_port =
    flag "--openflow-port" (optional_with_default 6633 int)
      ~doc:"int Port to listen on for OpenFlow switches. Defaults to 6633."
end


(*===========================================================================*)
(* COMMANDS                                                                  *)
(*===========================================================================*)

let default_spec =
  Command.Spec.empty

let run cmd =
  ignore (cmd ());
  never_returns (Async.Scheduler.go ())

exception ParseError of string * int * int * string

let parse_exn parser_function lexbuf (filename: string) =
  try
    parser_function Frenetic_Decide_Lexer.token lexbuf
  with
    | Parsing.Parse_error -> begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let char = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let token = Lexing.lexeme lexbuf in
      raise (ParseError (filename, line, char, token))
    end

let decide : Command.t =
  Command.basic_spec
    ~summary:"Invokes decision procedure with provided file."
    Command.Spec.(
      empty
      +> anon ("file" %: file)
    )
    (fun file () ->
      In_channel.with_file file ~binary:false ~f:(fun in_ch ->
        try
          let lexbuf = Lexing.from_channel in_ch in
          let formula = parse_exn Frenetic_Decide_Parser.formula_main lexbuf "" in
          let lhs, rhs = Frenetic_Decide_Ast.Formula.terms formula in
          ignore (Frenetic_Decide_Util.set_univ Frenetic_Decide_Ast.([Term.values lhs; Term.values rhs]));
          printf "%b\n%!" (Frenetic_Decide_Bisimulation.check_equivalent lhs rhs)
        with
        | ParseError (filename, line, char, token) ->
            printf "Parse error %s:%d%d: %s\n%!" filename line char token
      )
    )

let main : Command.t =
  Command.group
    ~summary:"Invokes the specified Frenetic module."
    [("decide", decide)]

let () =
  Command.run ~version: "5.0" ~build_info: "RWO" main
