open Core.Std
open Async.Std
open NetKAT_Types

module Controller = Async_NetKAT_Controller
module LC = NetKAT_LocalCompiler
module Field = NetKAT_FDD.Field
module Log = Async_OpenFlow.Log

type showable =
  (* usage: order
   * Shows the ordering that will be used on the next update.  *)
  | Ordering
  (* usage: policy
   * Shows the policy that is currently active.   *)
  | Policy
  (* usage: flow-table [policy]
   * Shows the flow-table produced by the specified policy.
   * If no policy is specified, the current policy is used. *)
  | FlowTable of (policy * string) option
  (* usage: help
   * Displays a helpful message. *)
  | Help

type command =
  (* usage: update <policy>
   * Compiles the specified policy using the current ordering
   * and updates the controller with the new flow-table *)
  | Update of (policy * string)
  (* usage: order <ordering>
   * Sets the order which the compiler will select field names when
   * constructing the BDD.
   * Valid orderings:
   *   heuristic - Uses a heuristic to select the order of fields
   *   default - Uses the default ordering as specified in NetKAT_LocalCompiler
   *   f_1 < f_2 [ < f_3 < ... < f_n ] - Given two or more fields, ensures the
   *                                     order of the specified fields is maintained. *)
  | Order of LC.order
  (* usage: exit
   * Exits the shell. *)
  | Exit
  (* usage: load <filename>
   * Loads the specified file as a policy and compiles it updating the controller with
   * the new flow table. *)
  | Load of string
  (* See showables for more details *)
  | Show of showable

module Parser = struct

    open MParser

    (* Parser for field as the to_string function displays it *or*
     * all lowercase for convenience. *)
    let field (f : Field.t) : (Field.t, bytes list) MParser.t =
      MParser.Tokens.symbol (Field.to_string f |> String.lowercase) <|>
      MParser.Tokens.symbol (Field.to_string f) >>
      return f

    (* Parser for any of the fields *)
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

    (* Parser that produces the Order command or Show Order command *)	    
    let order : (command, bytes list) MParser.t =
      Tokens.symbol "order" >> (
      (eof >> return (Show Ordering)) <|>
      (Tokens.symbol "heuristic" >> return (Order `Heuristic)) <|>
      (Tokens.symbol "default" >> return (Order `Default)) <|>
      (sep_by1 any_field (Tokens.symbol "<") >>= 
	 fun fields -> eof >> return (Order (`Static fields))))

    (* Mostly useless error message for parsing policies *)				 
    let string_of_position (p : Lexing.position) : string =
      sprintf "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

    (* Use the netkat parser to parse policies *)
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
   
    (* Parser for netkat policies *)
    let policy' : ((policy * string), bytes list) MParser.t =
      many_until any_char eof >>=
	(fun pol_chars ->
	 let pol_str = String.of_char_list pol_chars in
	 match parse_policy pol_str with
	 | Ok pol -> return (pol, pol_str)
	 | Error msg -> fail msg)      

    (* Parser for the Update command *)
    let update : (command, bytes list) MParser.t =
      Tokens.symbol "update" >> 
	policy' >>=
	(fun pol -> return (Update pol))

    (* Parser for the help command *)
    let help : (command, bytes list) MParser.t =
      Tokens.symbol "help" >> return (Show Help)

    (* Parser for the exit command *)
    let exit : (command, bytes list) MParser.t =
      Tokens.symbol "exit" >> return Exit

    (* Parser for the load command *)
    let load : (command, bytes list) MParser.t =
      Tokens.symbol "load" >>
	many_until any_char eof >>=
	(fun filename -> return (Load (String.of_char_list filename)))

    (* Parser for the policy command *)
    let policy : (command, bytes list) MParser.t =
      Tokens.symbol "policy" >> return (Show Policy)

    (* Parser for the flow-table command *)
    let flowtable : (command, bytes list) MParser.t =
      Tokens.symbol "flow-table" >>
	(eof >> return (Show (FlowTable None)) <|>
	(policy' >>= 
	   (fun pol -> return (Show (FlowTable (Some pol))))))

   (* Parser for commands *)
    let command : (command, bytes list) MParser.t =
      order <|>
	update <|>
	policy <|>
	help <|>
	flowtable <|>
	load <|>
	exit
end

(* For convenience *)
let compose f g x = f (g x)

(* Reference to the controller, this is set when the controller
 * is created and never changes. This is entirely for convenience. *)
let controller : (Controller.t option) ref = ref None

(* Reference to the ordering that is currently set.
 * Use heuristic ordering by default *)
let order : LC.order ref = ref `Heuristic

(* Prints the current ordering mode. *)
let print_order () : unit =
  match !order with
  | `Heuristic -> print_endline "Ordering Mode: Heuristic"
  | `Default -> print_endline "Ordering Mode: Default"
  | `Static fields ->
     let strs = List.rev (List.map fields Field.to_string) in
     let cs = String.concat ~sep:" < " strs in
     printf "Ordering Mode: %s\n%!" cs

(* Convenience function that checks that an ordering doesn't contain
 * duplicates. This is used in favor of List.contains_dup so a better
 * error message can be produced *)
let rec check_duplicates (fs : Field.t list) (acc : Field.t list) : bool =
  match fs with
  | [] -> false
  | (f::rest) -> 
     if List.mem acc f 
     then 
       (printf "Invalid ordering: %s < %s" (Field.to_string f) (Field.to_string f);
	false)
     else check_duplicates rest (f::acc)

(* Given an ordering, sets the order reference.
 * If a Static ordering is given with duplicates, the ordering
 * is not updated and an error message is printed *)
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
     if check_duplicates ls [] then ()
     else
     let curr_order = match !order with
                      | `Heuristic -> Field.all_fields
		      | `Default -> Field.all_fields
		      | `Static fields -> fields
     in
     let removed = List.filter curr_order (compose not (List.mem ls)) in
     (* Tags all specified Fields at the highest priority *)
     let new_order = List.append (List.rev ls) removed in
     order := (`Static new_order); 
     Controller.set_order (uw !controller) (`Static new_order);
     print_order ()

(* A reference to the current policy and the associated string. *)
let policy : (policy * string) ref = ref (drop, "drop")

(* Prints the current policy *)
let print_policy () =
  match !policy with
    (_, p) -> printf "%s\n%!" p

(* Module for pretty printing flow tables *)
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

    (* Builds up a list of strings one for each pattern *)
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

    (* Given a flow, return a pair of list of strings where the first list
     * contains the strings of the pattern and the second list contains
     * the strings of the actions associated with the pattern. *)
    let to_entry (f : SDN_Types.flow) : (string list) * (string list) =
      let open SDN_Types in
      let open List in
      let pattern_list = pattern_list f.pattern in
      let action_list = map (concat (concat f.action)) string_of_action in
      (pattern_list, action_list)

    (* Pads a string with spaces so that it is atleast `len` characters. *)
    let pad (len : int) (e : string) : string =
      let padding_size = max 0 (len - (String.length e)) in
      let padding = String.make padding_size ' ' in
      String.concat [e; padding]

    (* Helper function *)
    let unwrap x = 
      match x with
      | None -> 0
      | Some x -> x

    (* Given a list of entries to be displayed in the table, calculate a pair
     * containing the max characters in a pattern string and action string *)
    let table_size (sw_id : switchId) (entries : ((string list) * (string list)) list) : int * int =
      let open List in
      let patterns = map entries fst |> concat in
      let actions = map entries snd |> concat in
      let max_p =  max_elt (map patterns String.length) (-) |> unwrap in
      let max_a = max_elt (map actions String.length) (-) |> unwrap in
      (max max_p ((Int64.to_string sw_id |> String.length) + 3 + (String.length "Pattern")), max max_a (String.length "Action"))

    (* Create the top edge of the table *)
    let top max_p max_a : string =
      let open Char in
      let fill = String.make (max_p + max_a + 5) '-' in
      Format.sprintf "+%s+\n" fill

    (* Create the bottom edge of the table *)
    let bottom max_p max_a : string=
      let fill = String.make (max_p + max_a + 5) '-' in
      Format.sprintf "+%s+\n" fill

    (* Create a divider between entries *)
    let div max_p max_a : string =
      let fill = String.make (max_p + max_a + 5) '-' in
      Format.sprintf "|%s|\n" fill

    (* Create the columns of the table *)
    let title sw_id max_p max_a : string =
      let pattern = pad max_p (Format.sprintf "%Ld | Pattern" sw_id) in
      let action = pad max_a "Action" in
      Format.sprintf "| %s | %s |\n" pattern action

    (* Create a row in the table *)
    let string_of_entry (max_p : int) (max_a : int) (e : (string list) * (string list)) : string =
      let open List in
      let padded_patterns = map (fst e) (pad max_p) in 
      let padded_actions = map (snd e) (pad max_a) in 
      let blank_action = String.make max_a ' ' in
      let blank_pattern = String.make max_p ' ' in
      let rec helper pats acts acc =
	match pats, acts with
	| [], [] -> if (length acc) = 1
		    then (Format.sprintf "| %s | %s |\n" blank_pattern blank_action) :: acc
		    else acc
	| (p::ps), [] ->
	   let acc' = (Format.sprintf "| %s | %s |\n" p blank_action) :: acc in
	   helper ps [] acc'
	| [], (a::rest) -> 
	   let acc' = (Format.sprintf "| %s | %s |\n" blank_pattern a) :: acc in
	   helper [] rest acc'
	| (p::ps), (a::rest) -> 
	   let acc' = (Format.sprintf "| %s | %s |\n" p a) :: acc in
	   helper ps rest acc'
      in 
      helper padded_patterns padded_actions [(div max_p max_a)]
      |> rev |> String.concat

    (* Given a switch id and a flowTable, returns an ascii flowtable *)
    let string_of_table (sw_id : switchId) (tbl : flowTable) : string =
      let entries = List.map tbl to_entry in
      let (max_p, max_a) = table_size sw_id entries in
      let t = (top max_p max_a) in
      let l = (title sw_id max_p max_a) in
      let entry_strings = List.map entries (string_of_entry max_p max_a) in
      let b = bottom max_p max_a in
      String.concat (t :: l :: (List.append entry_strings [b]))

    (* Given a policy, returns a pretty ascii table for each switch *)		  
    let string_of_policy ?(order=`Heuristic) (pol : policy) : string =
      let bdd = LC.compile ~order:order pol in
      (* TODO: Get switch numbers *)
      let switches = NetKAT_Misc.switches_of_policy pol in
      let switches' = if List.is_empty switches then [0L] else switches in	
      let tbls = List.map switches' (fun sw_id -> LC.to_table sw_id bdd |> string_of_table sw_id) in
      String.concat ~sep:"\n\n" tbls
      
    (* Given a policy, print the flowtables associated with it *)  
    let print (pol : (policy * string) option) : unit =
      let (p, str) =
	match pol with
	| None -> !policy
	| Some x -> x
      in 
      printf "%s%!" (string_of_policy p)

end

let parse_command (line : string) : command option = 
  match (MParser.parse_string Parser.command line []) with
  | Success command -> Some command
  | Failed (msg, e) -> (print_endline msg; None)

let help =
  String.concat ~sep:"\n" [
  "";
  "commands:";
  "  order               - Display the ordering that will be used when compiling.";
  "  order <ordering>    - Changes the order in which the compiler selects fields.";
  "";
  "                        orderings: heuristic";
  "                                   default";
  "                                   f_1 < f_2 [ < f_3 < ... < f_n ]";
  "";
  "                        fields: Switch, Location, EthSrc, EthDst, Vlan, VlanPcP,";
  "                                EthType, IPProto, IP4Src, IP4Dst, TCPSrcPort,";
  "                                TCPDstPort";
  "";
  "  policy              - Displays the policy that is currently active.";
  "";
  "  flow-table [policy] - Displays the flow-table produced by the specified policy.";
  "                        If no policy is specified, the current policy is used.";
  "";
  "  update <policy>     - Compiles the specified policy using the current ordering";
  "                        and updates the controller with the resulting flow-table.";
  "";
  "  load <filename>     - Loads a policy from the specified file, compiles it, and";
  "                        updates the controller with the resulting flow-table.";
  "";
  "  help                - Displays this message.";
  "";
  "  exit                - Exits Frenetic Shell.";
  ""
  ]

let print_help () : unit =
  printf "%s\n%!" help

(* Loads a policy from a file and updates the controller *)
let load_file (filename : string) (pol_writer : policy Pipe.Writer.t) : unit =
  try
    let open In_channel in
    let chan = create filename in
    let policy_string = input_all chan in
    let pol = Parser.parse_policy policy_string in
    close chan;
    match pol with
    | Ok p -> 
       policy := (p, policy_string);
       printf "%s\n%!" policy_string;
       Pipe.write_without_pushback pol_writer p
    | Error msg -> print_endline msg
  with
  | Sys_error msg -> printf "Load failed: %s\n%!" msg

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
		  | Some (Load filename) -> load_file filename pol_writer
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
  printf "Frenetic Shell v 0.0\n%!";
  printf "Type `help` for a list of commands\n%!";
  match args with
  | [] ->
    let pol_writer = start_controller () in
    let _ = repl pol_writer in
    ()
  | _ -> (printf "Invalid arguments to shell.\n"; Shutdown.shutdown 0)

