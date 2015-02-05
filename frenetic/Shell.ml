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
  | FlowTable of (policy * string) option
  | Help

type command =
  | Update of (policy * string)
  | Order of LC.order
  | Show of showable
  | Exit

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

    let policy' : ((policy * string), bytes list) MParser.t =
      many_until any_char eof >>=
	(fun pol_chars ->
	 let pol_str = String.of_char_list pol_chars in
	 match parse_policy pol_str with
	 | Ok pol -> return (pol, pol_str)
	 | Error msg -> fail msg)      

    let update : (command, bytes list) MParser.t =
      Tokens.symbol "update" >> 
	policy' >>=
	(fun pol -> return (Update pol))

    let help : (command, bytes list) MParser.t =
      Tokens.symbol "help" >> return (Show Help)

    let exit : (command, bytes list) MParser.t =
      Tokens.symbol "exit" >> return Exit

    let policy : (command, bytes list) MParser.t =
      Tokens.symbol "policy" >> return (Show Policy)

    let flowtable : (command, bytes list) MParser.t =
      Tokens.symbol "flow-table" >>
	(eof >> return (Show (FlowTable None)) <|>
	(policy' >>= 
	   (fun pol -> return (Show (FlowTable (Some pol))))))

    let command : (command, bytes list) MParser.t =
      order <|>
	update <|>
	policy <|>
	help <|>
	flowtable <|>
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

let policy : (policy * string) ref = ref (drop, "drop")
let print_policy () =
  match !policy with
    (_, p) -> printf "Current policy: %s\n%!" p

module Table = struct
    open SDN_Types
    open Packet

    let vlan (x : int) : string = 
      Format.sprintf "Vlan = %d" x

    let vlanpcp (x : dlVlanPcp) : string =
      Format.sprintf "VlanPcp = %d" x

    let ethType (x : dlTyp) : string =
      let extra = if x = 0x800 then " (ip)" 
		  else if x = 0x806 then " (arp)"
		  else ""
      in
      Format.sprintf "EthType = 0x%x%s" x extra

    let ipProto (x : nwProto) : string =
      let extra = match x with
	| 0x01 -> " (icmp)"
	| 0x02 -> " (igmp)"
	| 0x06 -> " (tcp)"
	| 0x11 -> " (udp)"
	| _ -> ""
      in
      Format.sprintf "ipProto = 0x%x%s" x extra

    let ethSrc (x : dlAddr) : string =
      Format.sprintf "EthSrc = %s" (Packet.string_of_mac x)

    let ethDst (x : dlAddr) : string =
      Format.sprintf "EthDst = %s" (Packet.string_of_mac x)

    let ip4src (x : Pattern.Ip.t) : string =
      Format.sprintf "IP4Src = %s" (Pattern.Ip.string_of x)

    let ip4dst (x : Pattern.Ip.t) : string =
      Format.sprintf "IP4Dst = %s" (Pattern.Ip.string_of x)

    let tcpSrcPort (x : tpPort) : string =
      Format.sprintf "TCPSrcPort = %d" x

    let tcpDstPort (x : tpPort) : string =
      Format.sprintf "TCPDstPort = %d" x

    let inPort (x : portId) : string =
      Format.sprintf "In Port = %lu" x

    let check (string_of : 'a -> string) 
	      (x : 'a option) 
	      (acc : string list) : string list =
      match x with
      | None -> acc
      | Some x' -> (string_of x') :: acc

    let pattern_list (p : SDN_Types.Pattern.t) : string list =
      check ethSrc p.dlSrc [] |>
	check ethDst p.dlDst |>
	check ethType p.dlTyp |>
	check vlan p.dlVlan |>
	check vlanpcp p.dlVlanPcp |>
	check ip4src p.nwSrc |>
	check ip4dst p.nwDst |>
	check ipProto p.nwProto |>
	check tcpSrcPort p.tpSrc |>
	check tcpDstPort p.tpDst |>
	check inPort p.inPort

    let entry (f : SDN_Types.flow) : (string list) * (string list) =
      let open SDN_Types in
      let open List in
      let pattern_list = pattern_list f.pattern in
      let action_list = map (concat (concat f.action)) string_of_action in
      (pattern_list, action_list)

    let pad (len : int) (e : string) : string =
      let padding_size = len - (String.length e) in
      let padding = String.make padding_size ' ' in
      String.concat [e; padding]

    let unwrap x = 
      match x with
      | None -> 0
      | Some x -> x

    let table_size (entries : ((string list) * (string list)) list) : int * int =
      let open List in
      let patterns = map entries fst |> concat in
      let actions = map entries snd |> concat in
      let max_p =  max_elt (map patterns String.length) (-) |> unwrap in
      let max_a = max_elt (map actions String.length) (-) |> unwrap in
      (max max_p (String.length "Pattern"), max max_a (String.length "Action"))

    let bar max_p max_a =
      printf "|%s|\n" (String.make (max_p + max_a + 5) '-')

    let title max_p max_a =
      let pattern = pad max_p "Pattern" in
      let action = pad max_a "Action" in
      printf "| %s | %s |\n" pattern action

    let print_entry (max_p : int) (max_a : int) (e : (string list) * (string list)) : unit =
      let open List in
      let padded_patterns = map (fst e) (pad max_p) in 
      let padded_actions = map (snd e) (pad max_a) in 
      let rec helper pats acts =
	match pats, acts with
	| [], [] -> ()
	| (p::ps), [] -> 
	   printf "| %s | %s |\n" p (String.make max_a ' ');
	   helper ps []
	| [], (a::rest) -> 
	   printf "| %s | %s |\n" (String.make max_p ' ') a;
	   helper [] rest
	| (p::ps), (a::rest) -> 
	   printf "| %s | %s |\n" p a;
	   helper ps rest 
      in 
      bar max_p max_a;
      helper padded_patterns padded_actions

    let print (pol : (policy * string) option) : unit =
      let (p, str) =
	match pol with
	| None -> !policy
	| Some x -> x
      in 
      let bdd = LC.compile ~order:!order p in
      let tbl = LC.to_table 0L bdd in
      let entries = List.map tbl entry in
      let (max_p, max_a) = table_size entries in
      printf "Policy: %s\n%!" str;
      bar max_p max_a;
      title max_p max_a;
      let _ = List.map entries (print_entry max_p max_a) in
      bar max_p max_a;
      printf "%!"

end

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
		  | Some (Show Help) -> print_help ()
		  | Some (Show (FlowTable t)) -> Table.print t
		  | Some (Update (pol, pol_str)) -> 
		     policy := (pol, pol_str); 
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

