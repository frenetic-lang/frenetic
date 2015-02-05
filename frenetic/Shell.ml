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
  (* See showables for more details *)
  | Show of showable

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

let rec check_duplicates (fs : Field.t list) (acc : Field.t list) : bool =
  match fs with
  | [] -> false
  | (f::rest) -> 
     if List.mem acc f 
     then 
       (printf "Invalid ordering: %s < %s" (Field.to_string f) (Field.to_string f);
	false)
     else check_duplicates rest (f::acc)

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

	
    let top max_p max_a : string =
      let open Char in
      let fill = String.make (max_p + max_a + 5) '-' in
      Format.sprintf "+%s+\n" fill

    let bottom max_p max_a : string=
      let fill = String.make (max_p + max_a + 5) '-' in
      Format.sprintf "+%s+\n" fill

    let div max_p max_a : string =
      let fill = String.make (max_p + max_a + 5) '-' in
      Format.sprintf "|%s|\n" fill

    let title max_p max_a : string =
      let pattern = pad max_p "Pattern" in
      let action = pad max_a "Action" in
      Format.sprintf "| %s | %s |\n" pattern action

    let string_of_entry (max_p : int) (max_a : int) (e : (string list) * (string list)) : string =
      let open List in
      let padded_patterns = map (fst e) (pad max_p) in 
      let padded_actions = map (snd e) (pad max_a) in 
      let rec helper pats acts acc =
	match pats, acts with
	| [], [] -> acc
	| (p::ps), [] ->
	   let acc' = (Format.sprintf "| %s | %s |\n" p (String.make max_a ' ')) :: acc in
	   helper ps [] acc'
	| [], (a::rest) -> 
	   let acc' = (Format.sprintf "| %s | %s |\n" (String.make max_p ' ') a) :: acc in
	   helper [] rest acc'
	| (p::ps), (a::rest) -> 
	   let acc' = (Format.sprintf "| %s | %s |\n" p a) :: acc in
	   helper ps rest acc'
      in 
      helper padded_patterns padded_actions [(div max_p max_a)]
      |> rev |> String.concat

    let string_of_table tbl : string =
      let entries = List.map tbl entry in
      let (max_p, max_a) = table_size entries in
      let t = (top max_p max_a) in
      let l = (title max_p max_a) in
      let entry_strings = List.map entries (string_of_entry max_p max_a) in
      let b = bottom max_p max_a in
      String.concat (t :: l :: (List.append entry_strings [b]))
      
      

    let print (pol : (policy * string) option) : unit =
      let (p, str) =
	match pol with
	| None -> !policy
	| Some x -> x
      in 
      let bdd = LC.compile ~order:!order p in
      let tbl = LC.to_table 0L bdd in
      printf "Policy: %s\n%!" str;
      printf "%s%!" (string_of_table tbl)

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
  "  help                - Displays this message.";
  "";
  "  exit                - Exits Frenetic Shell.";
  ""
  ]

let print_help () : unit =
  printf "%s\n%!" help

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
  printf "Frenetic Shell v 0.0\n%!";
  printf "Type `help` for a list of commands\n%!";
  match args with
  | [] ->
    let pol_writer = start_controller () in
    let _ = repl pol_writer in
    ()
  | _ -> (printf "Invalid arguments to shell.\n"; Shutdown.shutdown 0)

