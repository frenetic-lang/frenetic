open Core.Std
open Async.Std
open NetKAT_Types

module Field = NetKAT_FDD.Field

module Log = Async_OpenFlow.Log

type ordering = 
  | Heuristic
  | Static of Field.t list

let id x = x

let compose f g x = f (g x)

(* Use heuristic ordering by default *)
let order : ordering ref = ref Heuristic

let print_order () : unit =
  match !order with
  | Heuristic -> print_endline "Ordering Mode: Heuristic"
  | Static fields ->
     let strs = List.rev (List.map fields Field.to_string) in
     let cs = String.concat ~sep:" < " strs in
     Printf.printf "Ordering Mode: %s\n%!" cs

let set_order (o : ordering) : unit = 
  match o with
  | Heuristic -> order := Heuristic; print_order ()
  | Static ls ->
     let curr_order = match !order with
                      | Heuristic -> Field.all_fields
		      | Static fields -> fields
     in
     let removed = List.filter curr_order (compose not (List.mem ls)) in
     let new_order = List.append ls removed in
     order := (Static new_order); print_order ()
  
  

let field_from_string (opp_str : string) : Field.t option = 
  match (String.lowercase opp_str) with
  | "switch" -> Some Switch
  | "location" -> Some Location
  | "ethsrc" -> Some EthSrc
  | "ethdst" -> Some EthDst
  | "vlan" -> Some Vlan
  | "vlanpcp" -> Some VlanPcp
  | "ethtype" -> Some EthType
  | "ipproto" -> Some IPProto
  | "ip4src" -> Some IP4Src
  | "ip4dst" -> Some IP4Dst
  | "tcpsrcport" -> Some TCPSrcPort
  | "tcpdstport" -> Some TCPDstPort
  | _ -> None

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

type showable =
  | Ordering

type command =
  | Update of policy
  | Order of ordering
  | Show of showable
  | Exit
  | Help

let with_error (msg : string) 
	       (to_string : 'a -> string) 
	       (f : 'a -> 'b option) 
	       (x : 'a) : 'b option =
  match f x with
  | Some result -> Some result
  | None -> 
     Printf.printf "%s: %s\n%!" msg (to_string x);
     None


(* Given a string of the format
   x_0 < x_1 < ... < x_n
   where x_i is an orderable field
   returns a list containing the fields in order
   descending order.
 *)
let parse_order (line : string) : (Field.t list) option = 
  let fields = String.split line '<' in
  let trimmed = List.map fields (compose String.lstrip String.rstrip) in
  let orders = List.map trimmed (with_error "Invalid ordering field" id field_from_string) in
  let rec helper (acc : Field.t list) (rest : (Field.t option) list) =
    match rest with
    | [] -> Some acc
    | (None::_) -> None
    | (Some x)::xs -> 
       if (List.mem acc x) 
       then (Printf.printf "Invalid ordering: %s < %s\n%!" (Field.to_string x) (Field.to_string x); None)
       else helper (x::acc) xs
  in
  helper [] orders

let parse_command (line : string) : command option = 
  match (compose String.lstrip String.rstrip) (String.lowercase line) with
  | "help" -> Some Help
  | "exit" -> Some Exit
  | "order" -> Some (Show Ordering)
  | "order default" -> Some (Order (Static []))
  | "order heuristic" -> Some (Order Heuristic)
  | _ -> (match String.lsplit2 line ~on:' ' with
    | Some ("order", order_str) ->
       (match parse_order order_str with
	| Some order -> Some (Order (Static order))
	| None -> None)
    | Some ("update", pol_str) ->
      (match parse_policy pol_str with
       | Ok pol -> Some (Update pol)
       | Error msg -> (print_endline msg; None))
    | _ -> None)

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
		  | Some Help -> print_help ()
		  | Some (Update pol) -> Pipe.write_without_pushback pol_writer pol
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
    (Async_NetKAT_Controller.start app () >>= fun ctrl ->
     Async_NetKAT_Controller.disable_discovery ctrl) in
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
