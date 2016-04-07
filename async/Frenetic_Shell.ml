open Core.Std
open Async.Std
open Frenetic_NetKAT

module Controller = Frenetic_NetKAT_Controller.Make
module Comp = Frenetic_NetKAT_Compiler
module Field = Frenetic_Fdd.Field
module Log = Frenetic_Log

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
  | Order of Comp.order
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
   * Loads the specified file as a policy and compiles it updating the controller with
   * the new flow table. *)
  | Load of string
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
      try
        Ok (Frenetic_NetKAT_Parser.policy_of_string pol_str)
      with Camlp4.PreCast.Loc.Exc_located (error_loc,x) ->
        Error (
          sprintf "Error: %s\n%s"
          (Camlp4.PreCast.Loc.to_string error_loc)
          (Exn.to_string x))

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

    (* Parser for the exit command *)
    let quit : (command, bytes list) MParser.t =
      Tokens.symbol "quit" >> return Quit

    (* Parser for the load command *)
    let load : (command, bytes list) MParser.t =
      Tokens.symbol "load" >>
	many_until any_char eof >>=
	(fun filename -> return (Load (String.of_char_list filename)))

    (* Parser for the policy command *)
    let policy : (command, bytes list) MParser.t =
      Tokens.symbol "policy" >> return (Show Policy)

    (* Parser for the remove_tail_drops command *)
    let remove_tail_drops : (command, bytes list) MParser.t =
      Tokens.symbol "remove_tail_drops" >> return ToggleRemoveTailDrops

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
  remove_tail_drops <|>
	load <|>
	exit <|>
  quit

end

(* For convenience *)
let compose f g x = f (g x)

(* TODO(jcollard): The cache flag here is actually a problem. Changing ordering won't work as expected. *)
let current_compiler_options = ref { Comp.default_compiler_options with cache_prepare = `Keep }

let set_field_order ord : unit =
  current_compiler_options := { !current_compiler_options with field_order = ord }

(* Prints the current ordering mode. *)
let print_order () : unit =
  printf "Ordering Mode: %s\n%!" (Comp.field_order_to_string (!current_compiler_options).field_order)

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
let set_order (o : Comp.order) : unit =
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
     let removed = List.filter curr_order (compose not (List.mem ls)) in
     (* Tags all specified Fields at the highest priority *)
     let new_order = List.append (List.rev ls) removed in
     set_field_order (`Static new_order);
     print_order ()

let toggle_remove_tail_drops () =
  let current_setting = (!current_compiler_options).remove_tail_drops in
  current_compiler_options := { !current_compiler_options with remove_tail_drops = not current_setting };
  printf "Remove Tail Drops: %B\n%!" (!current_compiler_options).remove_tail_drops

(* A reference to the current policy and the associated string. *)
let policy : (policy * string) ref = ref (drop, "drop")

(* Prints the current policy *)
let print_policy () =
  match !policy with
    (_, p) -> printf "%s\n%!" p

(* Given a policy, returns a pretty ascii table for each switch *)
let string_of_policy (pol : policy) : string =
  let bdd = Comp.compile_local ~options:(!current_compiler_options) pol in
  let switches = Frenetic_NetKAT_Semantics.switches_of_policy pol in
  let switches' = if List.is_empty switches then [0L] else switches in
  let tbls = List.map switches'
		      (fun sw_id -> Comp.to_table ~options:(!current_compiler_options) sw_id bdd |>
				      Frenetic_OpenFlow.string_of_flowTable ~label:(Int64.to_string sw_id)) in
  String.concat ~sep:"\n\n" tbls

(* Given a policy, print the flowtables associated with it *)
let print_policy_table (pol : (policy * string) option) : unit =
  let (p, str) =
    match pol with
    | None -> !policy
    | Some x -> x
  in
  printf "%s%!" (string_of_policy p)

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
let load_file (filename : string) : unit =
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
       don't_wait_for (Controller.update_policy p)
    | Error msg -> print_endline msg
  with
  | Sys_error msg -> printf "Load failed: %s\n%!" msg

let rec repl () : unit Deferred.t =
  printf "frenetic> %!";
  Reader.read_line (Lazy.force Reader.stdin) >>= fun input ->
  let handle line =
    match line with
    | `Eof -> Shutdown.shutdown 0
    | `Ok line -> match parse_command line with
		  | Some Exit | Some Quit ->
		     print_endline "Goodbye!";
		     Shutdown.shutdown 0
		  | Some (Show Ordering) -> print_order ()
		  | Some (Show Policy) -> print_policy ()
		  | Some (Show Help) -> print_help ()
		  | Some (Show (FlowTable t)) -> print_policy_table t
		  | Some (Update (pol, pol_str)) ->
		     policy := (pol, pol_str);
         don't_wait_for (Controller.update_policy pol)
		  | Some (Load filename) -> load_file filename
		  | Some (Order order) -> set_order order
      | Some (ToggleRemoveTailDrops) -> toggle_remove_tail_drops ()
		  | None -> ()
  in handle input; repl ()

let log_file = "frenetic.log"

let main (openflow_port : int) () : unit =
  Log.set_output [Async.Std.Log.Output.file `Text log_file];
  printf "Frenetic Shell v 4.0\n%!";
  printf "Type `help` for a list of commands\n%!";
  Controller.start openflow_port;
  let _ = repl () in
  ()

