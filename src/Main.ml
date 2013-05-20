open Lwt
open Printf
open Unix
open Syntax.External

module Controller = Controller.Make(OpenFlow0x01_Platform)

let string_of_position p =
  let open Lexing in
  sprintf "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

(* Parses concrete NetCore *)
let parse_from_lexbuf lexbuf name =
  let open Lexing in
    try
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
      Parser.program Lexer.token lexbuf
    with
      |  Failure "lexing: empty token" ->
        failwith (sprintf "lexical error at %s"
                    (string_of_position lexbuf.lex_curr_p))
      | Parser.Error ->
        failwith (sprintf "parse error at %s; unexpected token %s"
                    (string_of_position lexbuf.lex_curr_p)
                    (lexeme lexbuf))

let parse_from_chan cin name =
  parse_from_lexbuf (Lexing.from_channel cin) name


let policy = ref Empty

let () =
  Arg.parse
    [ ]
    (fun filename -> policy := parse_from_chan (open_in filename) filename)
    "Usage: netcore FILENAME"

let main () = 
  let stream, push = Lwt_stream.create () in  
  push (Some !policy);
  (* JNF: kind of a hack that we have to call this function :-( *)
  OpenFlow0x01_Platform.init_with_port 6633; 
  Controller.start_controller stream  
      
let _ =
  Sys.catch_break true;
  try 
    Lwt_main.run (main ())
  with exn -> 
    Misc.Log.printf "[main] exception: %s\n%s\n%!" 
      (Printexc.to_string exn) 
      (Printexc.get_backtrace ());
    exit 1
