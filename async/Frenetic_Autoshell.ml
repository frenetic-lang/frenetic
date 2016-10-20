open Core.Std
open Async.Std
open Frenetic_NetKAT
open Frenetic_Network
open Frenetic_Circuit_NetKAT

module Fabric = Frenetic_Fabric
module CoroNet = Frenetic_Topology.CoroNet
module Compiler = Frenetic_NetKAT_Compiler
module Log = Frenetic_Log

type fdd = Compiler.t
type automaton = Compiler.automaton

(** Utility functions and shorthands *)
let log_filename = "frenetic.log"
let log = Log.printf
let (>>|) = Result.(>>|)
let keep_cache = { Compiler.default_compiler_options
                   with cache_prepare = `Keep }

type loc = (switchId * portId)

type source =
  | String of string
  | Filename of string

type element =
  | Policy of policy
  | Fabric of policy
  | Circuit of circuit
  | Topology of policy

type configuration = { policy     : policy
                     ; fdd        : fdd option
                     ; automaton  : automaton option
                     ; ingresses  : loc list
                     ; egresses   : loc list
                     }

let new_config = { policy    = Filter True
                 ; fdd       = None
                 ; automaton = None
                 ; ingresses = []
                 ; egresses  = []
                 }

type fabric = { circuit : (config * loc list * loc list) option
              ; config  : configuration }

type state = { mutable naive    : configuration option
             ; mutable fabric   : fabric option
             ; mutable edge     : configuration option
             ; mutable topology : policy option
             }

let state = { naive    = None
            ; fabric   = None
            ; edge     = None
            ; topology = None
            }

type state_part =
  | SNaive
  | SFabric
  | SEdge
  | STopology
  | SCircuit

type config_part =
  | CPolicy
  | CFdd
  | CAuto
  | CIngress
  | CEgress

type load =
  | LNaive    of source * loc list * loc list
  | LFabric   of source * loc list * loc list
  | LCircuit  of source * loc list * loc list
  | LCoronet  of source * string list * string list
  | LTopo     of source

type compile =
  | CLocal                      (* Compile the naive policy as a local policy *)
  | CGlobal                     (* Compile the naive policy as a global policy *)
  | CFabric                     (* Compile the fabric as a local policy *)
  | CCircuit                    (* Compile the circuit to a fabric, & compile that *)
  | CEdge                       (* Compile the naive policy atop the fabric *)

type synthesize =
  | SGeneric
  | SOptical
  | SSMT

type install =
  | INaive  of switchId list
  | IFabric of switchId list
  | IEdge

type show =
  | SPart of state_part
  | STable of state_part * switchId

type command =
  | Load of load
  | Compile of compile
  | Synthesize of synthesize
  | Install of install
  | Show of show
  | Path of source
  | Blank
  | Quit

(** Useful modules, mostly for code clarity *)
module Parser = struct

  (** Monadic Parsers for the command line *)
  open MParser

  module Tokens = MParser_RE.Tokens

  let symbol = Tokens.symbol

  (* Parser for integer lists *)
  let int_list : (int list, bytes list) MParser.t =
    (char '[' >> many_until (many_chars_until digit (char ';')) (char ']') >>=
     (fun ints -> return (List.map ints ~f:Int.of_string)))

  let switch_list : (switchId list, bytes list) MParser.t =
    int_list >>= fun ints ->
    return (List.map ints ~f:Int64.of_int_exn)

  (* Parser for lists of locations: (switch, port) pairs written as sw:pt *)
  let loc_list : (loc list, bytes list) MParser.t =
    (char '[' >> many_until (
        many_chars_until digit (char ':') >>=
        (fun swid -> many_chars_until digit (char ';') >>=
          (fun ptid -> return ((Int64.of_string swid),
                               (Int32.of_string ptid)))))
        (char ']') >>=
     (fun ints -> return ints))

  (* Parser for list of strings *)
  let string_list : (string list, bytes list) MParser.t =
    (char '[' >> many_until (many_chars_until letter (char ';')) (char ']') >>=
     (fun strings -> return strings))

  (* Parser for sources *)
  let source : (source, bytes list) MParser.t =
    (char '"' >> many_chars_until any_char (char '"') >>=
     (fun string -> return ( String string) ) ) <|>
    (many_chars (alphanum <|> (any_of "./_-")) >>=
     (fun w -> return (Filename w)))

  (* Parser for sources with ingress & egress locations *)
  let guarded_source : ((source * loc list * loc list), bytes list) MParser.t =
    (source >>=
     (fun pol -> blank >> loc_list >>=
       (fun ings -> blank >> loc_list >>=
         (fun egs ->
            return (pol, ings, egs)))))

  let coronet : ((source * string list * string list), bytes list) MParser.t =
    source >>= fun s -> blank >>
    string_list >>= fun east -> blank >>
    string_list >>= fun west ->
    return (s, east, west)

  (* Parser for parts of the state *)
  let state_part : (state_part, bytes list) MParser.t =
      (symbol "policy"   >> return SNaive) <|>
      (symbol "fabric"   >> return SFabric) <|>
      (symbol "edge"     >> return SEdge) <|>
      (symbol "topology" >> return STopology) <|>
      (symbol "circuit"  >> return SCircuit)

  (* Parser for load command *)
  let load : (command, bytes list) MParser.t =
    symbol "load" >> (
      (symbol "policy" >> guarded_source >>=
       fun (s,i,o) -> return ( LNaive(s,i,o) )) <|>
      (symbol "fabric" >> guarded_source >>=
       fun (s,i,o) -> return ( LFabric(s,i,o) )) <|>
      (symbol "circuit" >> guarded_source >>=
       fun (s,i,o) -> return ( LCircuit(s,i,o) )) <|>
      (symbol "coronet" >> coronet >>=
       fun (s,e,w) -> return (LCoronet(s,e,w))) <|>
      (symbol "topology" >>
       source >>= fun s -> return (LTopo s))) >>=
    fun l -> return ( Load l )

  (* Parser for the compile command *)
  let compile : (command, bytes list) MParser.t =
    symbol "compile" >> (
      (symbol "local"   >> return CLocal) <|>
      (symbol "global"  >> return CGlobal) <|>
      (symbol "fabric"  >> return CFabric) <|>
      (symbol "circuit" >> return CCircuit) <|>
      (symbol "edge"    >> return CEdge)) >>=
    fun c -> return( Compile c )

  (* Parser for the synthesize command. Maybe should be merged with compile. *)
  let synthesize : (command, bytes list) MParser.t =
    symbol "synthesize" >> (
      (symbol "generic" >> return SGeneric) <|>
      (symbol "optical" >> return SOptical) <|>
      (symbol "smt" >> return SSMT)) >>=
    fun s -> return ( Synthesize s )

  (* Parser for the install command *)
  let install : (command, bytes list) MParser.t =
    symbol "install" >> (
      (symbol "policy" >> switch_list >>= fun swids ->
           return( INaive swids )) <|>
      (symbol "fabric" >> switch_list >>= fun swids ->
       return( IFabric swids )) <|>
      (symbol "edge"   >> return IEdge)) >>=
    (fun i -> return( Install i))

  (* Parser for the show command *)
  let show : (command, bytes list) MParser.t =
    symbol "show" >> (
      (state_part >>= fun s -> return( Show( SPart s ))) <|>
      (symbol "table" >> many_chars_until digit blank >>=
       fun id ->
       let swid = Int64.of_string id in
       state_part >>=
       fun sp -> return( Show( STable(sp, swid) ))))

  (* Parser for the path command *)
  let path: (command, bytes list) MParser.t =
    symbol "path" >> (
      source >>= fun s -> return (Path s))

  (* Parser for a blank line *)
  let blank : (command, bytes list) MParser.t =
    eof >> return Blank

  (* Parser for the quit command *)
  let quit : (command, bytes list) MParser.t =
    (symbol "exit" <|> symbol "quit") >> return Quit

  let command : (command, bytes list) MParser.t =
    load    <|>
    compile <|>
    synthesize <|>
    install <|>
    show    <|>
    path    <|>
    blank   <|>
    quit

  (** Non-Monadic parsers for the information that the shell can
  manipulate. Mostly just wrappers for parsers from the rest of the Frenetic
  codebase. *)

  (* Use the netkat parser to parse policies *)
  let policy (pol_str : string) : (policy, string) Result.t =
    try
      Ok (Frenetic_NetKAT_Parser.policy_of_string pol_str)
    with Camlp4.PreCast.Loc.Exc_located (error_loc,x) ->
      Error (sprintf "Error: %s\n%s"
               (Camlp4.PreCast.Loc.to_string error_loc)
               (Exn.to_string x))

end

module Source = struct
  let to_string (s:source) : (string, string) Result.t = match s with
    | String s -> Ok s
    | Filename f ->
      try
        let chan = In_channel.create f in
        Ok (In_channel.input_all chan)
      with Sys_error msg -> Error msg

  let to_policy (s:source) : (policy, string) Result.t =
    match to_string s with
    | Ok s -> Parser.policy s
    | Error e -> print_endline e; Error e
end

let string_of_policy = Frenetic_NetKAT_Pretty.string_of_policy

let string_of_loc (s,p) =
  sprintf "%Ld:%ld" s p

let string_of_locs ls =
  sprintf "[%s]"
    (String.concat ~sep:"; " (List.map ls string_of_loc))

let string_of_guarded_source s i o =
  let s' = match s with
    | String s
    | Filename s -> s in
  sprintf "%s [%s] [%s] "
    s'
    ( String.concat ~sep:"; " (List.map i ~f:string_of_loc) )
    ( String.concat ~sep:"; " (List.map i ~f:string_of_loc) )

let string_of_config c =
  sprintf "Policy:\n%s\nIngresses: %s\nEgresses: %s\n"
    (string_of_policy c.policy)
    (string_of_locs c.ingresses)
    (string_of_locs c.egresses)

let config (s:source) (i:loc list) (o:loc list) : (configuration, string) Result.t =
  Source.to_policy s >>| fun pol ->
  { new_config with policy = pol ;
                    ingresses = i ;
                    egresses = o }

let get_config_fdd (c:configuration option) = match c with
  | Some c -> begin match c.fdd with
      | Some f -> Ok f
      | None -> Error "Policy has not been compiled to FDD." end
  | None -> Error "Policy has not been loaded and compiled."

let get_state_fdd (s:state_part) = match s with
  | SNaive -> get_config_fdd state.naive
  | SFabric -> begin match state.fabric with
      | Some f -> get_config_fdd (Some f.config)
      | None -> Error "No fabric loaded and compiled." end
  | SEdge -> get_config_fdd state.edge
  | SCircuit -> Error "Circuits must be compiled to a fabric first."
  | STopology -> begin match state.topology with
      | None -> Error "No topology defined."
      | Some t -> Error "Topology has no FDD." end

let load_coronet (fn:string) (east:string list) (west:string list) =
  let net,id_tbl = CoroNet.from_csv_file fn in
  let mn = CoroNet.Pretty.to_mininet ~prologue_file:"examples/linc-prologue.txt"
      ~link_class:( Some "LINCLink" ) net in
  let nk = string_of_policy (CoroNet.Pretty.to_netkat net) in
  try
    let show = function
      | Some p -> CoroNet.CoroPath.to_string p
      | None -> "" in
    let paths = CoroNet.cross_connect net id_tbl east west in
    let src     = sprintf "Source: %s" fn in
    let mininet = sprintf "Mininet:\n%s\n" mn in
    let netkat  = sprintf "NetKAT:\n%s\n" nk in
    let body = List.map paths ~f:(fun (shortest,local,across) ->
        sprintf "Shortest: [%s]\nLocal First: [%s]\nAcross First: [%s]"
          (show shortest) (show local) (show across)) in
    Ok (String.concat ~sep:"\n" ( src::mininet::netkat::body ))
  with
  | CoroNet.NonexistentNode s ->
    Error (sprintf "No node %s in topology read from %s\n" s fn)
  | CoroNet.CoroPath.UnjoinablePaths s ->
    Error (sprintf "Unjoinable paths while loading %s: %s\n" fn s)

let load (l:load) : (string, string) Result.t =
  let (>>=) = Result.(>>=) in
  match l with
  | LNaive    (s,i,o) ->
    config s i o >>| fun c ->
    state.naive <- Some c ;
    log "Loaded naive configuration:\n%s\n" (string_of_config c);
    "Loaded new naive policy."
  | LFabric   (s,i,o) ->
    config s i o >>| fun c ->
    state.fabric <- Some { circuit = None ; config = c };
    log "Loaded fabric configuration:\n%s\n" (string_of_config c);
    "Loaded new fabric policy"
  | LCircuit  (s,i,o) ->
    Source.to_policy s >>= fun p ->
    config_of_policy p >>= fun c ->
    validate_config c  >>= fun c ->
    let pol = local_policy_of_config c in
    let conf = { new_config with policy = pol; ingresses = i; egresses = o } in
    state.fabric <- Some { circuit = Some (c,i,o); config = conf };
    log "Loaded circuit policy:\n%s\n" (string_of_policy p);
    log "Compiled circuit configuration:\n%s\n" (string_of_config conf);
    Ok "Loaded new circuit policy"
  | LTopo s ->
    Source.to_policy s >>| fun t ->
    state.topology <- Some t;
    log "Loaded topology:\n%s\n" (string_of_policy t);
    "Loaded new topology"
  | LCoronet (s,east,west) ->
    (* TODO(basus): Allow coronet topologies to be loaded from strings as well *)
    match s with
    | String s -> Error "Coronet topologies must be loaded from files"
    | Filename fn -> load_coronet fn east west

let compile_local (c:configuration) =
  let open Compiler in
  try
    let fdd = compile_local ~options:keep_cache c.policy in
    log "Compiled local policy to FDD:\n%s\n"
      (Frenetic_Fdd.FDD.to_string fdd);
    Ok { c with fdd = Some fdd }
  with Non_local ->
    Error "Given policy is not local. It contains links."

let compile_global (c:configuration) =
  let open Compiler in
  let automaton = compile_to_automaton ~options:keep_cache c.policy in
  log "Compiled global policy to automaton:\n%s\n"
      (Compiler.automaton_to_string automaton);
  let fdd = compile_from_automaton automaton in
  log "Compiled global policy to FDD:\n%s\n"
    (Frenetic_Fdd.FDD.to_string fdd);
  Ok { c with fdd = Some fdd; automaton = Some automaton }

let compile_fabric (f:fabric) =
  log "Compiling fabric\n";
  compile_local f.config >>| fun c' ->
  { f with config = c' }

let compile_circuit (f:fabric) = match f.circuit with
  | None -> Error "No circuit specified. Please load with `load circuit` command."
  | Some (c,i,o) ->
    let l = local_policy_of_config c in
    log "Compiling circuit policy: %s\n%!" (string_of_policy l);
    let conf = { new_config with policy    = l;
                                 ingresses = i;
                                 egresses  = o } in
    log "Compiled circuit to configuration:\n%s\n" (string_of_config conf);
    log "Compiling circuit configuration\n";
    compile_local conf >>| fun c' ->
    { f with config = c' }

let compile_edge c f topo =
  let open Compiler in
  let (fpol,fins,fouts) = (f.config.policy, f.config.ingresses, f.config.egresses) in

  let naive     = Fabric.assemble c.policy topo c.ingresses c.egresses in
  let fabric    = Fabric.assemble fpol topo fins fouts in
  let parts     = Fabric.Dyad.of_policy naive in
  let fab_parts = Fabric.Dyad.of_policy fabric in
  let ins, outs = Fabric.retarget parts fab_parts topo in

  let ingress = Frenetic_NetKAT_Optimize.mk_big_union ins in
  let egress  = Frenetic_NetKAT_Optimize.mk_big_union outs in
  let edge    = Frenetic_NetKAT.Union (ingress, egress) in

  log "Assembled naive policy:\n%s\n" (string_of_policy naive);
  log "Assembled fabric policy:\n%s\n" (string_of_policy fabric);

  log "Policy alpha/beta pairs:\n";
  List.iter parts (fun s -> log "%s\n" (Fabric.Dyad.to_string s));

  log "Fabric alpha/beta pairs:\n";
  List.iter fab_parts (fun s -> log "%s\n" (Fabric.Dyad.to_string s));

  log "Retargeted ingress policy:\n%s\n" (string_of_policy ingress);
  log "Retargeted egress policy:\n%s\n" (string_of_policy egress);

  let edge_fdd = compile_local ~options:keep_cache edge in
  Ok { new_config with policy = edge;
                       ingresses = c.ingresses; egresses = c.egresses;
                       fdd = Some edge_fdd }

let compile (c:compile) : (string, string) Result.t = match c with
  | CLocal -> begin match state.naive with
    | None -> Error "No policy specified. Please load with `load policy` command."
    | Some c ->
      compile_local c >>| fun c' ->
      state.naive <- Some c';
      "Local naive policy compiled successfully";
    end
  | CGlobal -> begin match state.naive with
    | None -> Error "No policy specified. Please load with `load policy` command."
    | Some c ->
      compile_global c >>| fun c' ->
      state.naive <- Some c';
      "Global naive policy compiled successfully" end
  | CFabric -> begin match state.fabric with
      | None -> Error "No fabric specified. Please load with `load fabric` command."
      | Some f ->
        compile_fabric f >>| fun f' ->
        state.fabric <- Some f';
        "Fabric compiled successfully" end
  | CCircuit -> begin match state.fabric with
      | None -> Error "No circuits specified. Please load with `load circuit` command."
      | Some f ->
        compile_circuit f >>| fun f' ->
        state.fabric <- Some f';
        "Circuit compiled successfully" end
  | CEdge -> begin match state.naive, state.fabric,state.topology with
      | Some c, Some f, Some t ->
        compile_edge c f t >>| fun c ->
        state.edge <- Some c;
        "Edge policies compiled successfully"
      | _ -> Error "Edge compilation requires naive policy, fabric and topology"
    end

let synthesize s : (string, string) Result.t =
  let (>>=) = Result.(>>=) in
  let open Frenetic_Synthesis in
  Random.init 1337;
  begin match s with
    | SGeneric -> Ok ( module Make(Generic) : SYNTH )
    | SOptical -> Ok ( module Make(Optical) : SYNTH )
    | SSMT ->
      (* This is just an example to test the synthesis code. Much of this needs
         to be parameterized. *)
      let open Z3 in
      let places = TestsOnly(Fabric,
                             ( Frenetic_Fdd.FieldSet.of_list [Switch; Location] )) in
      let restraint = And(Adjacent, places) in
      let decider = mk_decider restraint in
      Ok ( module Make(struct
            let decide   = decider
            let choose   = Optical.choose
            let generate = Optical.generate
          end) : SYNTH )
  end >>= fun s ->
  match state.naive, state.fabric,state.topology with
  | Some c, Some f, Some t ->
    let module S = (val s) in
    let fabric = Fabric.assemble f.config.policy t
        f.config.ingresses f.config.egresses in
    let policy = Fabric.assemble c.policy t c.ingresses c.egresses in
    let edge = S.synthesize policy fabric t in

    log "Pre-synthesis user policy:\n%s\n"   (string_of_policy policy);
    log "Pre-synthesis fabric policy:\n%s\n" (string_of_policy fabric);
    log "Synthesized edge policy:\n%s\n"     (string_of_policy edge);

    let edge_fdd = Compiler.compile_local ~options:keep_cache edge in
    state.edge <- Some { new_config with
                         policy = edge;
                         ingresses = c.ingresses; egresses = c.egresses;
                         fdd = Some edge_fdd };
    Ok "Edge policies compiled successfully"
  | _ -> Error "Edge compilation requires naive policy, fabric and topology"


let post (uri:Uri.t) (body:string) =
  let open Cohttp.Body in
  try_with (fun () ->
  Cohttp_async.Client.post ~body:(`String body) uri >>=
  fun (_,body) -> Cohttp_async.Body.to_string body) >>=
  (function
    | Ok s -> return( Ok s )
    | Error e -> return( Error( Exn.to_string e )))

let install_fdd ?(host="localhost") ?(port=6634) (fdd:fdd) (swids:switchId list) =
  List.map swids ~f:(fun swid ->
      let path = String.concat ~sep:"/" ["install"; Int64.to_string swid] in
      let uri = Uri.make ~host:host ~port:port ~path:path () in
      try
        let table =  Compiler.to_table swid fdd in
        let json = (Frenetic_NetKAT_Json.flowTable_to_json table) in
        let body = Yojson.Basic.to_string json in
        log "Installing flowtable on switch %Ld:\n%s\n" swid
          (Frenetic_OpenFlow.string_of_flowTable table);
        post uri body
      with Not_found ->
        return (Error (sprintf "No table found for swid %Ld\n%!" swid)))

let install_naive c swids = match c.fdd with
  | Some fdd -> install_fdd fdd swids
  | None -> [ return ( Error "Please compile a naive policy first" )]

let install_config ?(switches=[]) c = match c.fdd, switches with
  | None, _ -> [ return ( Error "Please compile the appropriate policy first" )]
  | Some fdd, [] ->
    let open List in
    let swids = dedup (rev_append
                         (map c.ingresses fst) (map c.egresses fst)) in
    install_fdd fdd swids
  | Some fdd, sws ->
    install_fdd fdd sws

let install (i:install) = match i with
  | INaive swids -> begin match state.naive with
      | Some c -> install_naive c swids
      | None -> [ return ( Error "Please load and compile a naive policy first" )] end
  | IEdge -> begin match state.edge with
      | Some c -> install_config c
      | None -> [ return ( Error "Please load and compile a fabric and naive policy first" )]
    end
  | IFabric swids -> begin match state.fabric with
      | Some f -> install_config ~switches:swids f.config
      | None -> [ return ( Error "Please load and compile a fabric first" )]
    end

let show (s:show) = match s with
  | SPart SNaive -> begin match state.naive with
      | Some c -> print_endline (string_of_config c)
      | None -> print_endline "No naive policy defined." end
  | SPart SFabric -> begin match state.fabric with
      | Some f -> print_endline (string_of_config f.config)
      | None -> print_endline "No fabric defined." end
  | SPart SEdge -> begin match state.edge with
      | Some c ->
        print_endline "Showing edge configuration";
        print_endline (string_of_config c)
      | None -> print_endline "No edge defined." end
  | SPart STopology -> begin match state.topology with
      | Some t -> print_endline (string_of_policy t)
      | None   -> print_endline "No topology defined" end
  | SPart SCircuit -> begin match state.fabric with
      | Some f -> begin match f.circuit with
          | Some c ->
            let c',i,o = c in
            printf "Ingresses:\n%s\n" (string_of_locs i);
            printf "Egresses:\n%s\n" (string_of_locs o);
            print_endline (Frenetic_Circuit_NetKAT.string_of_config c')
          | None   -> print_endline "Fabric defined directly, without circuit"
        end
      | None -> print_endline "No circuit defined" end
  | STable ( part, swid ) -> begin match (get_state_fdd part) with
      | Ok fdd ->
        let table = Compiler.to_table swid fdd in
        let label = sprintf "Switch: %Ld " swid in
        print_endline (Frenetic_OpenFlow.string_of_flowTable ~label:label table)
      | Error s ->
        print_endline s end

let path s : unit = match Source.to_string s with
  | Error e -> print_endline e
  | Ok s ->
    begin match Fabric.Path.of_string s, state.fabric,state.topology with
      | Ok paths, Some f, Some topo ->
        (* TODO(basus): Update the edge configuration in the state *)
        let (fpol,fins,fouts) = (f.config.policy, f.config.ingresses, f.config.egresses) in
        let fabric      = Fabric.assemble fpol topo fins fouts in
        let fab_streams = Fabric.Dyad.of_policy fabric in
        let ins, outs = Fabric.Path.project paths fab_streams topo in
        printf "\nIngress policy:\n";
        List.iter ins (fun p -> printf "%s\n" (string_of_policy p));
        printf "\nEgress policy:\n";
        List.iter outs (fun p -> printf "%s\n" (string_of_policy p));
      | Ok paths, _, _ -> print_endline "Pathfinding needs both fabric and topology"
      | Error e, _, _ -> print_endline e end

let print_result r : unit = match r with
  | Ok msg  -> printf "Success: %s\n" msg
  | Error e -> printf "Error: %s\n" e

let print_deferred_results rs =
  Deferred.all rs >>>
  List.iter ~f:(function
      | Error e -> printf "Error: %s\n" e
      | Ok m -> printf "Success: %s\n" m)

let command (com:command) = match com with
  | Load l ->
    load l |> print_result
  | Compile c ->
    compile c |> print_result
  | Synthesize s ->
    synthesize s |> print_result
  | Install i ->
    install i |> print_deferred_results
  | Show s -> show s
  | Path s -> path s
  | Quit ->
    print_endline "Goodbye!";
    Shutdown.shutdown 0
  | Blank -> ()

let handle (line: string) : unit =
  match (MParser.parse_string Parser.command line []) with
  | Success com     -> command com
  | Failed (msg, e) -> print_endline msg

let rec repl ?(prompt="powershell> ") () : unit Deferred.t =
  printf "%s%!" prompt;
  Reader.read_line (Lazy.force Reader.stdin) >>=
  ( fun line -> match line with
      | `Eof ->
        return ( Shutdown.shutdown 0 )
      | `Ok line ->
        handle line;
        repl ())

let main () : unit =
  Log.set_output [Async.Std.Log.Output.file `Text log_filename];
  printf "Frenetic PowerShell v 1.0\n%!";
  printf "Type `help` for a list of commands\n%!";
  ignore(repl ())
