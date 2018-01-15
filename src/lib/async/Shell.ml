open Core
open Async
open Frenetic_netkat.Syntax

module Netkat = Frenetic_netkat
module Controller = NetKAT_Controller.Make(OpenFlow0x01_Plugin)
module Field = Netkat.Fdd.Field

type showable =
  (* usage: order
   * Shows the ordering that will be used on the next update.  *)
  | Ordering
  (* usage: policy
   * Shows the policy that is currently active.   *)
  | Policy
  (* usage: flow-table [policy]
   * Shows the flow-table produced by current policy *)
  | FlowTable
  (* usage: help
   * Displays a helpful message. *)
  | Help

type command =
  (* usage: update <policy>
   * Compiles the specified local policy using the current ordering
   * and updates the controller with the new flow-table *)
  | Update of (policy * string)
  (* usage: update-global <policy>
   * Compiles the specified global policy using the current ordering
   * and updates the controller with the new flow-table *)
  | UpdateGlobal of (policy * string)
  (* usage: order <ordering>
   * Sets the order which the compiler will select field names when
   * constructing the BDD.
   * Valid orderings:
   *   heuristic - Uses a heuristic to select the order of fields
   *   default - Uses the default ordering as specified in Frenetic_netkat.LocalCompiler
   *   f_1 < f_2 [ < f_3 < ... < f_n ] - Given two or more fields, ensures the
   *                                     order of the specified fields is maintained. *)
  | Order of Netkat.Local_compiler.order
  (* usage: remove_tail_drops
   * Remove any drop rules at the end of each flow table.  Toggles setting.
  *)
  | ToggleRemoveTailDrops
  (* usage: exit
   * Exits the shell. *)
  | Exit
  (* usage: quit
   * Exits the shell. *)
  | Quit
  (* usage: load <filename>
   * Loads the specified file as a local policy and compiles it updating
     the controller with the new flow table. *)
  | Load of string
  (* usage: load-global <filename>
   * Like load, but for global policy *)
  | LoadGlobal of string
  (* See showables for more details *)
  | Show of showable


module Parser = struct

    open MParser

    module Tokens = MParser_RE.Tokens

    (* Parser for field as the to_string function displays it *or*
     * all lowercase for convenience. *)
    let field (f : Field.t) : (Field.t, bytes list) MParser.t =
      Tokens.symbol (Field.to_string f |> String.lowercase) <|>
      Tokens.symbol (Field.to_string f) >>
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
      Ok (Frenetic_netkat.Parser.pol_of_string pol_str)

    (* Parser for netkat policies *)
    let policy' : ((policy * string), bytes list) MParser.t =
      many_until any_char eof >>= fun pol_chars ->
      let pol_str = String.of_char_list pol_chars in
      match parse_policy pol_str with
      | Ok pol -> return (pol, pol_str)
      | Error msg -> fail msg

    (* Parser for the Update command *)
    let update : (command, bytes list) MParser.t =
      Tokens.symbol "update" >>
    	policy' >>=
    	(fun pol -> return (Update pol))

    (* Parser for the Update global command *)
    let update_global : (command, bytes list) MParser.t =
      Tokens.symbol "update-global" >>
      policy' >>=
      (fun pol -> return (UpdateGlobal pol))

    (* Parser for the help command *)
    let help : (command, bytes list) MParser.t =
      Tokens.symbol "help" >> return (Show Help)

    (* Parser for the exit command *)
    let exit : (command, bytes list) MParser.t =
      Tokens.symbol "exit" >> return Exit

    (* Parser for the exit command *)
    let quit : (command, bytes list) MParser.t =
      Tokens.symbol "quit" >> return Quit

    (* Parser for the load command *)
    let load : (command, bytes list) MParser.t =
      Tokens.symbol "load" >>
    	many_until any_char eof >>=
    	(fun filename -> return (Load (String.of_char_list filename)))

    (* Parser for the load-global command *)
    let load_global : (command, bytes list) MParser.t =
      Tokens.symbol "load-global" >>
      many_until any_char eof >>=
      (fun filename -> return (LoadGlobal (String.of_char_list filename)))

    (* Parser for the policy command *)
    let policy : (command, bytes list) MParser.t =
      Tokens.symbol "policy" >> return (Show Policy)

    (* Parser for the remove_tail_drops command *)
    let remove_tail_drops : (command, bytes list) MParser.t =
      Tokens.symbol "remove_tail_drops" >> return ToggleRemoveTailDrops

    (* Parser for the flow-table command *)
    let flowtable : (command, bytes list) MParser.t =
      Tokens.symbol "flow-table" >>
    	eof >> 
      return (Show FlowTable)

   (* Parser for commands *)
    let command : (command, bytes list) MParser.t =
      order <|>
      update_global <|>
    	update <|>
    	policy <|>
    	help <|>
    	flowtable <|>
      remove_tail_drops <|>
      load_global <|>
    	load <|>
    	exit <|>
      quit

end


(* TODO(jcollard): The cache flag here is actually a problem. Changing ordering won't work as expected. *)
let current_compiler_options =
  ref { Netkat.Local_compiler.default_compiler_options with cache_prepare = `Keep }

let set_field_order ord : unit =
  current_compiler_options := { !current_compiler_options with field_order = ord }

(* Prints the current ordering mode. *)
let print_order () : unit =
  (!current_compiler_options).field_order
  |> Netkat.Local_compiler.field_order_to_string
  |> printf "Ordering Mode: %s\n%!"

(* Convenience function that checks that an ordering doesn't contain
 * duplicates. This is used in favor of List.contains_dup so a better
 * error message can be produced *)
let rec check_duplicates (fs : Field.t list) (acc : Field.t list) : bool =
  match fs with
  | [] -> false
  | (f::rest) ->
     if List.mem acc f ~equal:Field.equal
     then
       (printf "Invalid ordering: %s < %s" (Field.to_string f) (Field.to_string f);
	false)
     else check_duplicates rest (f::acc)

(* Given an ordering, sets the order reference.
 * If a Static ordering is given with duplicates, the ordering
 * is not updated and an error message is printed *)
let set_order (o : Netkat.Local_compiler.order) : unit =
  match o with
  | `Heuristic ->
     set_field_order `Heuristic;
     print_order ()
  | `Default ->
     set_field_order `Default;
     print_order ()
  | `Static ls ->
     if check_duplicates ls [] then ()
     else
     let curr_order = match (!current_compiler_options).field_order with
      | `Heuristic -> Field.all
      | `Default -> Field.all
      | `Static fields -> fields
     in
     let removed = List.filter curr_order (Fn.compose not (List.mem ls ~equal:Field.equal)) in
     (* Tags all specified Fields at the highest priority *)
     let new_order = List.append (List.rev ls) removed in
     set_field_order (`Static new_order);
     print_order ()

let toggle_remove_tail_drops () =
  let current_setting = (!current_compiler_options).remove_tail_drops in
  current_compiler_options := { !current_compiler_options with remove_tail_drops = not current_setting };
  printf "Remove Tail Drops: %B\n%!" (!current_compiler_options).remove_tail_drops

(* A reference to the current policy and the associated string. *)
let policy : [`Local of (policy * string) | `Global of (policy * string) ] ref = 
  ref (`Local (drop, "drop"))

let compile_current () = match !policy with
  | `Local (p,_) ->
    Netkat.Local_compiler.compile ~options:(!current_compiler_options) p
  | `Global (p,_) ->
    Netkat.Global_compiler.compile ~options:(!current_compiler_options) p

(* Prints the current policy *)
let print_policy () =
  match !policy with
  | `Local (_, p) -> printf "Local policy:\n%s\n%!" p
  | `Global (_, p) -> printf "Global policy:\n%s\n%!" p

(* Print the flowtables associated with the current policy *)
let print_policy_table () : unit =
  let pol = match !policy with `Local (p,_) | `Global (p,_) -> p in
  let fdd = compile_current () in
  let switches = Frenetic_netkat.Semantics.switches_of_policy pol in
  (if List.is_empty switches then [0L] else switches)
  |> List.map ~f:(fun sw -> 
    Netkat.Local_compiler.to_table ~options:(!current_compiler_options) sw fdd 
    |> Frenetic_kernel.OpenFlow.string_of_flowTable ~label:(Int64.to_string sw))
  |> String.concat ~sep:"\n\n"
  |> printf "%s%!"


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
  "  flow-table          - Displays the flow-table produced by the specified policy.";
  "                        If no policy is specified, the current policy is used.";
  "";
  "  update <policy>     - Compiles the specified local policy using the current";
  "                        ordering and updates the controller with the resulting";
  "                        flow-table.";
  "";
  "  update-global <pol> - Like update, but with a global policy.";
  "";
  "  load <file>         - Loads local policy from the specified file, compiles it,";
  "                        and updates the controller with the resulting flow-table.";
  "";
  "  load-global <file>  - Like load, but with global policy.";
  "";
  "  remove_tail_drops   - Remove drop rules at the end of each flow-table.  Toggles ";
  "                        setting.";
  "";
  "  help                - Displays this message.";
  "";
  "  exit                - Exits Frenetic Shell.";
  "";
  "  quit                - Exits Frenetic Shell.  Equivalent to CTRL-D";
  ""
  ]

let print_help () : unit =
  printf "%s\n%!" help

(* Loads a policy from a file and updates the controller *)
let load_file (typ : [`Local | `Global]) (filename : string) : unit =
  try
    let open In_channel in
    let chan = create filename in
    let policy_string = input_all chan in
    let pol = Parser.parse_policy policy_string in
    close chan;
    match pol with
    | Ok p ->
      policy := begin match typ with 
        | `Local -> `Local (p, policy_string)
        | `Global -> `Global (p, policy_string)
      end;
      print_policy ();
      compile_current ()
      |> Controller.update_fdd
      |> don't_wait_for
    | Error msg -> print_endline msg
  with
  | Sys_error msg -> printf "Load failed: %s\n%!" msg

let rec repl () : unit Deferred.t =
  printf "frenetic> %!";
  Reader.read_line (Lazy.force Reader.stdin) >>= fun input ->
  let handle line =
    try match line with
    | `Eof -> Shutdown.shutdown 0
    | `Ok line -> match parse_command line with
		  | Some Exit | Some Quit ->
		     print_endline "Goodbye!";
		     Shutdown.shutdown 0
		  | Some (Show Ordering) -> print_order ()
		  | Some (Show Policy) -> print_policy ()
		  | Some (Show Help) -> print_help ()
		  | Some (Show FlowTable) -> print_policy_table ()
		  | Some (Update (pol, pol_str)) ->
		     policy := `Local (pol, pol_str);
         compile_current ()
         |> Controller.update_fdd
         |> don't_wait_for
      | Some (UpdateGlobal (pol, pol_str)) ->
         policy := `Global (pol, pol_str);
         compile_current ()
         |> Controller.update_fdd
         |> don't_wait_for
		  | Some (Load filename) -> load_file `Local filename
      | Some (LoadGlobal filename) -> load_file `Global filename
		  | Some (Order order) -> set_order order
      | Some (ToggleRemoveTailDrops) -> toggle_remove_tail_drops ()
		  | None -> ()
    with exn -> Location.report_exception Format.std_formatter exn
  in handle input; repl ()

let log_file = "frenetic.log"

let main (openflow_port : int) () : unit =
  Logging.set_output [Async.Log.Output.file `Text log_file];
  printf "Frenetic Shell v 4.0\n%!";
  printf "Type `help` for a list of commands\n%!";
  Controller.start openflow_port;
  let _ = repl () in
  ()

