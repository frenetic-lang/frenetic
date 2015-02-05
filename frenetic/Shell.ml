open Core.Std
open Async.Std
open NetKAT_Types

module Controller = Async_NetKAT_Controller
module LC = NetKAT_LocalCompiler
module Field = NetKAT_FDD.Field
module Log = Async_OpenFlow.Log

type showable =
  | Ordering
  | Policy

type command =
  | Update of policy * string
  | Order of LC.order
  | Show of showable
  | Exit
  | Help

module Parser = struct

    open MParser

    let field (f : Field.t) : (Field.t, bytes list) MParser.t =
      MParser.Tokens.symbol (Field.to_string f |> String.lowercase) <|>
      MParser.Tokens.symbol (Field.to_string f) >>
      return f

    let any_field : (Field.t, bytes list) MParser.t =
      field Field.Switch <|>
      field Field.Location <|>
      field Field.EthSrc <|>
      field Field.EthDst <|>
      field Field.Vlan <|>
      field Field.VlanPcp <|>
      field Field.EthType <|>
      field Field.IPProto <|>
      field Field.IP4Src <|>
      field Field.IP4Dst <|>
      field Field.TCPSrcPort <|>
      field Field.TCPDstPort

    let order : (command, bytes list) MParser.t =
      Tokens.symbol "order" >> (
      (eof >> return (Show Ordering)) <|>
      (Tokens.symbol "heuristic" >> return (Order `Heuristic)) <|>
      (Tokens.symbol "default" >> return (Order `Default)) <|>
      (sep_by1 any_field (Tokens.symbol "<") >>= 
	 fun fields -> eof >> return (Order (`Static fields))))

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

    let update : (command, bytes list) MParser.t =
      Tokens.symbol "update" >>
      many_until any_char eof >>=
	(fun pol_chars ->
	 let pol_str = String.of_char_list pol_chars in
	 match parse_policy pol_str with
	 | Ok pol -> return (Update (pol, pol_str))
	 | Error msg -> fail msg)

    let help : (command, bytes list) MParser.t =
      Tokens.symbol "help" >> return Help

    let exit : (command, bytes list) MParser.t =
      Tokens.symbol "exit" >> return Exit

    let policy : (command, bytes list) MParser.t =
      Tokens.symbol "policy" >> return (Show Policy)

    let command : (command, bytes list) MParser.t =
      order <|>
	update <|>
	policy <|>
	help <|>
	exit
end

let compose f g x = f (g x)

let controller : (Controller.t option) ref = ref None

(* Use heuristic ordering by default *)
let order : LC.order ref = ref `Heuristic

let print_order () : unit =
  match !order with
  | `Heuristic -> print_endline "Ordering Mode: Heuristic"
  | `Default -> print_endline "Ordering Mode: Default"
  | `Static fields ->
     let strs = List.rev (List.map fields Field.to_string) in
     let cs = String.concat ~sep:" < " strs in
     printf "Ordering Mode: %s\n%!" cs

let set_order (o : LC.order) : unit = 
  match o with
  | `Heuristic -> 
     order := `Heuristic; 
     Controller.set_order (uw !controller) `Heuristic;
     print_order ()
  | `Default -> 
     order := `Default;
     Controller.set_order (uw !controller) `Default;
     print_order ()
  | `Static ls ->
     let curr_order = match !order with
                      | `Heuristic -> Field.all_fields
		      | `Default -> Field.all_fields
		      | `Static fields -> fields
     in
     let removed = List.filter curr_order (compose not (List.mem ls)) in
     let new_order = List.append (List.rev ls) removed in
     order := (`Static new_order); 
     Controller.set_order (uw !controller) (`Static new_order);
     print_order ()

let policy : string ref = ref "drop"
let print_policy () =
  printf "Current policy: %s\n%!" !policy

let parse_command (line : string) : command option = 
  match (MParser.parse_string Parser.command line []) with
  | Success command -> Some command
  | Failed (msg, e) -> (print_endline msg; None)

let print_help () : unit =
  printf "Read source code for help.\n"

let rec repl (pol_writer : policy Pipe.Writer.t) : unit Deferred.t =
  printf "frenetic> %!";
  Reader.read_line (Lazy.force Reader.stdin) >>= fun input ->
  let handle line = 
    match line with
    | `Eof -> Shutdown.shutdown 0
    | `Ok line -> match parse_command line with
		  | Some Exit -> 
		     print_endline "Goodbye!";
		     Shutdown.shutdown 0
		  | Some (Show Ordering) -> print_order ()
		  | Some (Show Policy) -> print_policy ()
		  | Some Help -> print_help ()
		  | Some (Update (pol, pol_str)) -> 
		     policy := pol_str; 
		     Pipe.write_without_pushback pol_writer pol
		  | Some (Order order) -> set_order order
		  | None -> ()
  in handle input; repl pol_writer

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
    (Async_NetKAT_Controller.start app () >>= 
       (fun ctrl -> controller := Some ctrl;
	            Async_NetKAT_Controller.disable_discovery ctrl)) in
  pol_writer

let log_file = "frenetic.log"

let main (args : string list) : unit =
  Log.set_output [Async.Std.Log.Output.file `Text log_file];
  printf "Frenetic Shell\n%!";
  match args with
  | [] ->
    let pol_writer = start_controller () in
    let _ = repl pol_writer in
    ()
  | _ -> (printf "Invalid arguments to shell.\n"; Shutdown.shutdown 0)

