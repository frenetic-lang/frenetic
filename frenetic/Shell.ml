open Core.Std
open Async.Std
open NetKAT_Types

let string_of_position (p : Lexing.position) : string =
  sprintf "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

let parse_policy ?(name = "") (pol_str : string) : (policy, string) Result.t =
  let lexbuf = Lexing.from_string pol_str in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
  try
    Ok (NetKAT_Parser.program NetKAT_Lexer.token lexbuf)
  with
    | Failure "lexing: empty token" ->
      Error (sprintf "error lexing policy at %s" (string_of_position lexbuf.lex_curr_p))
    | Parsing.Parse_error ->
      Error (sprintf "error parsing policy at %s" (string_of_position lexbuf.lex_curr_p))

type command =
  | Update of policy
  | Help

let parse_command (line : string) : command option = match line with
  | "help" -> Some Help
  | _ -> (match String.lsplit2 line ~on:' ' with
    | Some ("update", pol_str) ->
      (match parse_policy pol_str with
       | Ok pol -> Some (Update pol)
       | Error msg -> (print_endline msg; None))
    | _ -> None)

let print_help () : unit Deferred.t =
  printf "Read source code for help.\n";
  return ()

let rec repl (pol_writer : policy Pipe.Writer.t) : unit Deferred.t =
  printf "frenetic> %!";
  Reader.read_line (Lazy.force Reader.stdin) >>= fun line ->
  match line with
  | `Eof -> return (Shutdown.shutdown 0)
  | `Ok line -> match parse_command line with
    | Some Help ->
      print_help () >>= fun () ->
      repl pol_writer
    | Some (Update pol) ->
      (Pipe.write_without_pushback pol_writer pol;
       repl pol_writer)
    | None -> repl pol_writer

let start_controller () : policy Pipe.Writer.t =
  let pipes = Async_NetKAT.PipeSet.empty in
  let (pol_reader, pol_writer) = Pipe.create () in
  let app = Async_NetKAT.Policy.create_async ~pipes:pipes drop
    (fun topo send () ->
       let pol_writer' = send.update in
       let _ = Pipe.transfer_id pol_reader pol_writer' in
       fun event ->
         printf "Got network event.\n%!";
         return None) in
  let () = don't_wait_for
    (Async_NetKAT_Controller.start app () >>= fun ctrl ->
     Async_NetKAT_Controller.disable_discovery ctrl) in
  pol_writer

let main (args : string list) : unit =
  printf "Frenetic Shell\n%!";
  match args with
  | [] ->
    let pol_writer = start_controller () in
    let _ = repl pol_writer in
    ()
  | _ -> (printf "Invalid arguments to shell.\n"; Shutdown.shutdown 0)
