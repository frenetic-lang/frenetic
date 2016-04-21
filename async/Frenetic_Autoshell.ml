open Core.Std
open Async.Std
open Frenetic_NetKAT
open Frenetic_Network

module Compiler = Frenetic_NetKAT_Compiler
module Log = Frenetic_Log

type source =
  | String of string
  | Filename of string

type element =
  | Policy   of policy
  | Topology of Net.Topology.t
  | Fabric   of (switchId, Frenetic_OpenFlow.flowTable) Hashtbl.t

type input =
  | IPolicy
  | ITopology
  | IFabric

type command =
  | Load of (input * source)
  | Compile
  | Fabric
  | Write of string
  | Exit

module Parser = struct

  (** Monadic Parsers for the command line *)
  open MParser

  module Tokens = MParser_RE.Tokens

  let symbol = Tokens.symbol

  (* Parser for sources *)
  let source : (source, bytes list) MParser.t =
    (char '"' >> many_chars_until any_char (char '"') >>=
     (fun string -> return ( String string) ) ) <|>
    (many_until any_char eof >>=
     (fun w -> return (Filename (String.of_char_list w))))

  (* Parser for the load command *)
  let loadable : (command, bytes list) MParser.t =
    symbol "load" >> (
      (symbol "policy"   >> source >>= (fun s -> return (Load (IPolicy, s)))) <|>
      (symbol "topology" >> source >>= (fun s -> return (Load (ITopology, s)))) <|>
      (symbol "fabric"   >> source >>= (fun s -> return (Load (IFabric, s)))))

  (* Parser for the compile command *)
  let compile : (command, bytes list) MParser.t =
    symbol "compile" >> return Compile

  (* Parser for the fabric command *)
  let fabric : (command, bytes list) MParser.t =
    symbol "fabric" >> return Fabric

  (* Parser for the write command *)
  let write : (command, bytes list) MParser.t =
    symbol "write" >>
      many_until any_char eof >>=
      (fun filename -> return (Write (String.of_char_list filename)))

  (* Parser for the exit command *)
  let exit : (command, bytes list) MParser.t =
    (symbol "exit" <|> symbol "quit") >> return Exit

  let command : (command, bytes list) MParser.t =
    loadable <|>
    compile  <|>
    fabric   <|>
    write    <|>
    exit

  (** Non-Monadic parsers for the information that the shell can
  manipulate. Mostly just wrappers for parsers from the rest of the Frenetic
  codebase. *)

  (* Use the netkat parser to parse policies *)
  let policy (pol_str : string) : (element, string) Result.t =
    try
      Ok (Policy (Frenetic_NetKAT_Parser.policy_of_string pol_str))
    with Camlp4.PreCast.Loc.Exc_located (error_loc,x) ->
      Error (sprintf "Error: %s\n%s"
               (Camlp4.PreCast.Loc.to_string error_loc)
               (Exn.to_string x))

end

let string_of_source (s:source) : string = match s with
  | String s -> s
  | Filename f ->
    let chan = In_channel.create f in
    In_channel.input_all chan

(* A reference to the current policy and the associated string. *)
let policy = ref drop

(* A reference to the current automaton and the associated policy. *)
let automaton = ref None

(* A reference to the current topoology and associated filename *)
let topology : Net.Topology.t option ref = ref None

(* A reference to the current fabric *)
let fabric : (switchId, Frenetic_OpenFlow.flowTable) Hashtbl.t ref = ref (Hashtbl.Poly.create ())

let load (l:input) (s:source) : (element, string) Result.t = match l with
  | IPolicy -> Parser.policy (string_of_source s)
  | ITopology -> begin match s with
      | Filename f -> Ok (Topology (Net.Parse.from_dotfile f))
      | _ -> Error "Topologies can only be loaded from DOT files" end
  | IFabric -> Error "Fabric loading unimplemented"

let compile (pol : policy) : Frenetic_NetKAT_Compiler.automaton =
  Frenetic_NetKAT_Compiler.compile_to_automaton pol

let mk_fabric (net : Net.Topology.t) :
    (switchId, Frenetic_OpenFlow0x01.flowMod list) Hashtbl.t =
  Frenetic_Fabric.vlan_per_port net

let write (at : Frenetic_NetKAT_Compiler.automaton) (filename:string) : unit =
  let string = Frenetic_NetKAT_Compiler.automaton_to_string at in
  Out_channel.write_all "%s\n%!" ~data:string

let parse_command (line : string) : command option =
  match (MParser.parse_string Parser.command line []) with
  | Success command -> Some command
  | Failed (msg, e) -> (print_endline msg; None)

let rec repl () : unit Deferred.t =
  printf "autoshell> %!";
  Reader.read_line (Lazy.force Reader.stdin) >>= fun input ->
  let handle line = match line with
    | `Eof -> Shutdown.shutdown 0
    | `Ok line -> match parse_command line with
      | Some (Load (l,s)) -> begin match load l s with
          | Ok (Policy p)   -> policy := p
          | Ok (Topology t) -> topology := Some t
          | Ok (Fabric f)   -> fabric := f
          | Error s         -> print_endline s end
      | Some Fabric -> begin match !topology with
          | Some net ->
            (* TODO(basus) : fix fabric generation interface *)
          ignore(mk_fabric net)
        | None -> printf "Please load a topology with the `load topology` command first."
      end
      | Some Compile ->
        let pol = !policy in
        automaton := Some (compile pol, pol)
      | Some (Write filename) -> begin match !automaton with
        | None -> print_endline "No compiled automaton available"
        | Some (at, _) -> write at filename end
      | Some Exit ->
	print_endline "Goodbye!";
	Shutdown.shutdown 0
      | None -> ()
  in handle input;
  repl ()

let main () : unit =
  Log.set_output [Async.Std.Log.Output.file `Text "frenetic.log"];
  printf "Frenetic Automaton Shell v 1.0\n%!";
  printf "Type `help` for a list of commands\n%!";
  let _ = repl () in
  ()


