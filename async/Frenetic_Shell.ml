open Core.Std
open Async.Std
open Frenetic_NetKAT

module Controller = Frenetic_NetKAT_Controller.Make(Frenetic_OpenFlow0x01_Plugin)
module Comp = Frenetic_NetKAT_Compiler
module DecideAst = Frenetic_Decide_Ast
module DecideDeriv = Frenetic_Decide_Deriv
module DecideLexer = Frenetic_Decide_Lexer
module DecideParser = Frenetic_Decide_Parser
module DecideUtil = Frenetic_Decide_Util
module Felix = Frenetic_Decide_Measurement
module FelixLexer = Frenetic_Decide_Lexer
module FelixParser = Frenetic_Decide_Parser
module Field = Frenetic_Fdd.Field
module Log = Frenetic_Log

(* Felix reads in a network specified by four kat files, {in,p,t,out}.kat, and
 * a single query file, q.query. This record contains these five filenames. *)
type felix_files = {
  in_: string;
  p:   string;
  t:   string;
  out: string;
  q:   string;
}

type showable =
  (* usage: help
   * Displays a helpful message. *)
  | Help

type command =
  (* usage: exit
   * Exits the shell. *)
  | Exit
  (* usage: decide <formula>
   * Run the decision procedure on <formula>. *)
  | Decide of string

module Parser = struct

    open MParser

    module Tokens = MParser_RE.Tokens

    (* Parser for the help command *)
    let help : (command, bytes list) MParser.t =
      Tokens.symbol "help" >> return (Show Help)

    (* Parser for the exit command *)
    let exit : (command, bytes list) MParser.t =
      Tokens.symbol "exit" >> return Exit

    (* Parser for the decide command *)
    let decide : (command, bytes list) MParser.t =
      Tokens.symbol "decide" >>
      many_until any_char eof >>= fun formula ->
      return (Decide (String.of_char_list formula))

    (* Parser for commands *)
    let command : (command, bytes list) MParser.t =
      help <|>
      exit <|>
      decide
end

(* For convenience *)
let compose f g x = f (g x)

(* TODO(jcollard): The cache flag here is actually a problem. Changing ordering won't work as expected. *)
let current_compiler_options = ref { Comp.default_compiler_options with cache_prepare = `Keep }

let print_list (xs: 'a list) (f: 'a -> string) : unit =
    List.iter xs ~f:(fun x -> printf "%s\n" (f x))

let print_points (points: DecideAst.point list) : unit =
    print_list points DecideAst.point_to_string

let term_to_points (t: DecideAst.Term.t) : DecideAst.point list =
  let module DerivTerm = DecideDeriv.BDDDeriv in
  let tvals = DecideAst.Term.values t in
  ignore (DecideUtil.set_univ [tvals]);
  let t' = DerivTerm.make_term (DecideAst.TermSet.singleton t) in
  let q_E = DerivTerm.get_e t' in
  let points = DerivTerm.EMatrix.fold q_E ~init:[] ~f:(fun a p -> p :: a) in
  List.filter points ~f:(fun (alpha, beta) ->
    let no_snowman pkt =
      let snowman = "☃" in
      DecideAst.FieldMap.fold ~init:true ~f:(
        fun ~key ~data b ->
          DecideUtil.Field.to_string key <> snowman &&
          DecideUtil.Value.to_string data <> snowman &&
          b
      ) pkt
    in
    no_snowman alpha && no_snowman beta
  )

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
  "  felix <in> <p> <t> <out> <q>";
  "                      - Compile Felix query q against network in,p,t,out";
  "";
  "  decide <formula>    - Decide NetKAT formula";
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

let rec repl () : unit Deferred.t =
  printf "frenetic> %!";
  Reader.read_line (Lazy.force Reader.stdin) >>= fun input ->
  let handle line =
    match line with
    | `Eof -> Shutdown.shutdown 0
    | `Ok line -> match parse_command line with
		  | Some Exit ->
		     print_endline "Goodbye!";
		     Shutdown.shutdown 0
		  | Some (Show Help) -> print_help ()
                  | Some (Decide formula) -> decide formula
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

