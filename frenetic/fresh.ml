open Core.Std
open Async.Std
open Frenetic_NetKAT
open Frenetic_Network
open Frenetic_Circuit_NetKAT

module Fabric = Frenetic_Fabric
module CoroNet = Frenetic_Topology.CoroNet
module CoroNode = Frenetic_Topology.CoroNode
module Compiler = Frenetic_NetKAT_Compiler
module Log = Frenetic_Log

type fdd = Compiler.t
type automaton = Compiler.automaton

exception ShortPath of string

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

type synthesize =
  | SNative
  | SGeneric
  | SOptical
  | SSMT
  | SSAT_E
  | SLP_E
  | SLP
  | SLP_W

module PathTable = Hashtbl.Make (struct
    type t = switchId * portId * switchId * portId [@@deriving sexp,compare]
    let hash = Hashtbl.hash
  end )

module Coronet = struct
  (** Module to hold state and functions for the Coronet examples *)

  type state = { mutable network : CoroNet.Topology.t option
               ; mutable names   : Frenetic_Topology.name_table
               ; mutable ports   : Frenetic_Topology.port_table
               ; mutable east    : string list
               ; mutable west    : string list
               ; mutable paths   : CoroNet.CoroPath.t list
               ; mutable classes : int option
               ; mutable waypoints : int option
               ; mutable switches : switchId list
               ; mutable fabric  : (policy * loc list) option
               ; mutable policy  : (policy * loc list) option
               ; mutable fibers  : CoroNet.Waypath.fiber list * CoroNet.Waypath.fiber list
               ; mutable preproc : Int64.t
               ; mutable result  : Frenetic_Synthesis.result Async.Std.Deferred.t
               ; mutable pointtbl : CoroNet.Waypath.pointtable
               ; pathtbl : ((loc * loc), switchId list) Hashtbl.t
               }

  let state = { network  = None
              ; names    = String.Table.create ~size:0 ()
              ; ports    = String.Table.create ~size:0 ()
              ; east     = []
              ; west     = []
              ; paths    = []
              ; switches = []
              ; classes  = None
              ; waypoints = None
              ; fibers   = ([], [])
              ; fabric   = None
              ; policy   = None
              ; preproc = 0L
              ; result   = Async.Std.Deferred.return (Filter False, [])
              ; pointtbl = Hashtbl.Poly.create ()
              ; pathtbl  = Hashtbl.Poly.create ()
              }

  type t =
    | CLoadTopo of source
    | CLoadPaths of source
    | CEast of string list
    | CWest of string list
    | CPath of string * string
    | CClasses of int
    | CWaypoints of int
    | CPeek
    | CSMT
    | CPreprocess
    | CSynthesize of synthesize

  let result (nodes:int) timings =
    let nodes_col = "Edge Nodes" in
    let nodes_val = sprintf "***%d" nodes in
    let columns = List.map timings ~f:fst in
    let times = List.map timings ~f:(fun t -> snd t |> Int64.to_string )in
    let header = String.concat ~sep:"\t" ( nodes_col::columns ) in
    let values = String.concat ~sep:"\t" ( nodes_val::times ) in
    [ header; values ]
end

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
  | LTopo     of source

type compile =
  | CLocal                      (* Compile the naive policy as a local policy *)
  | CGlobal                     (* Compile the naive policy as a global policy *)
  | CFabric                     (* Compile the fabric as a local policy *)
  | CCircuit                    (* Compile the circuit to a fabric, & compile that *)
  | CEdge                       (* Compile the naive policy atop the fabric *)

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
  | Coronet of Coronet.t
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
  let engines : (synthesize, bytes list) MParser.t =
    (symbol "native" >> return SNative) <|>
    (symbol "generic" >> return SGeneric) <|>
    (symbol "optical" >> return SOptical) <|>
    (symbol "smt" >> return SSMT) <|>
    (symbol "sate" >> return SSAT_E) <|>
    (symbol "lpe" >> return SLP_E) <|>
    (symbol "lpw" >> return SLP_W) <|>
    (symbol "lp" >> return SLP)

  let synthesize : (command, bytes list) MParser.t =
    symbol "synthesize" >> engines >>=
    fun e -> return ( Synthesize e )

  (* Parser for the Coronet command. *)
  let coronet : (command, bytes list) MParser.t =
    let open Coronet in
    symbol "coronet" >> (
      (symbol "synthesize" >> engines >>=
       fun e -> return (CSynthesize e)) <|>
      (symbol "preprocess" >> return CPreprocess) <|>
      (symbol "load" >>
       ((symbol "topo" >> source >>= fun s -> return ( CLoadTopo s )) <|>
        (symbol "paths" >> source >>= fun s -> return ( CLoadPaths s )))) <|>
      (symbol "east" >> string_list >>= fun e -> return ( CEast e )) <|>
      (symbol "west" >> string_list >>= fun w -> return ( CWest w )) <|>
      (symbol "peek" >> return CPeek) <|>
      (symbol "smt" >> return CSMT) <|>
      (symbol "classes" >> many_chars digit >>= fun c ->
       return (CClasses (Int.of_string c))) <|>
      (symbol "waypoints" >> many_chars digit >>= fun c ->
       return (CWaypoints (Int.of_string c))) <|>
      (symbol "path" >>
       many_chars alphanum >>= fun src -> spaces >>
       many_chars alphanum >>= fun dst ->
       return ( CPath (src, dst)))) >>=
    fun c -> return (Coronet c)

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
    coronet <|>
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

let result timings =
    let columns = List.map timings ~f:fst in
    let times = List.map timings ~f:(fun t -> snd t |> Int64.to_string )in
    let header = String.concat ~sep:"\t" columns in
    let values = String.concat ~sep:"\t" times in
    [ header; values ]

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
  let module A = Fabric.Assemblage in
  let (fpol,fins,fouts) = (f.config.policy, f.config.ingresses, f.config.egresses) in

  let naive     = A.assemble c.policy topo c.ingresses c.egresses in
  let fabric    = A.assemble fpol topo fins fouts in
  let parts     = A.to_dyads naive in
  let fab_parts = A.to_dyads fabric in
  let ins, outs = Fabric.retarget parts fab_parts topo in

  let ingress = Frenetic_NetKAT_Optimize.mk_big_union ins in
  let egress  = Frenetic_NetKAT_Optimize.mk_big_union outs in
  let edge    = Frenetic_NetKAT.Union (ingress, egress) in

  log "Assembled naive policy:\n%s\n" (string_of_policy (A.program naive));
  log "Assembled fabric policy:\n%s\n" (string_of_policy (A.program fabric ));

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
    | SNative  -> Ok ( module MakeStrict(Optical) : DYADIC )
    | SGeneric -> Ok ( module MakeStrict(Generic) : DYADIC )
    | SOptical -> Ok ( module MakeStrict(Optical) : DYADIC )
    | SLP -> Ok ( module LP_Predicated )
    | SSAT_E -> Ok ( module SAT_Endpoints )
    | SLP_E  -> Ok ( module LP_Endpoints )
    | SLP_W -> Ok ( module LP_Endpoints )
    | SSMT ->
      (* This is just an example to test the synthesis code. Much of this needs
         to be parameterized. *)
      let open Z3 in
      let places = TestsOnly(Fabric,
                             ( Frenetic_Fdd.FieldSet.of_list [Switch; Location] )) in
      let restraint = And(Adjacent, places) in
      let decider = mk_dyad_decider restraint in
      Ok ( module MakeStrict(struct
            type t = Fabric.Dyad.t
            let decide   = decider
            let choose   = Optical.choose
            let generate = Optical.generate
          end) : DYADIC )
  end >>= fun s ->
  match state.naive, state.fabric,state.topology with
  | Some c, Some f, Some t ->
    let module S = (val s) in
    let module A = Fabric.Assemblage in
    let fabric = A.assemble f.config.policy t
        f.config.ingresses f.config.egresses in
    let policy = A.assemble c.policy t c.ingresses c.egresses in
    ( try
        let edge, timings = S.synthesize (A.to_dyads policy) (A.to_dyads fabric) t in

        log "Pre-synthesis user policy:\n%s\n"   (string_of_policy (A.program policy));
        log "Pre-synthesis fabric policy:\n%s\n" (string_of_policy (A.program fabric ));
        log "Synthesized edge policy:\n%s\n"     (string_of_policy edge);

        let edge_fdd = Compiler.compile_local ~options:keep_cache edge in
        state.edge <- Some { new_config with
                             policy = edge;
                             ingresses = c.ingresses; egresses = c.egresses;
                             fdd = Some edge_fdd };
        let report = result timings in
        let msg = String.concat ~sep:"\n"
            ("Edge policies compiled successfully"::report) in
        Ok msg
      with
      | Frenetic_LP.LPParseError e -> Error (sprintf "Cannot parse LP Solution: %s\n" e))
  | _ -> Error "Edge compilation requires naive policy, fabric and topology"

let cpeek () =
  let state = Coronet.state in
  match Async.Std.Deferred.peek state.result with
  | None -> Error "No result available yet"
  | Some (edge, [gen_time;synth_time]) ->
    let open Frenetic_Time in
    let ids = state.switches in
    let compiled = Compiler.compile_local ~options:keep_cache edge in
    let num_flows = List.fold ids ~init:0 ~f:(fun num id ->
        let flows = Frenetic_NetKAT_Compiler.to_table id compiled in
        (List.length flows) + num) in
    let nodes = ( List.length state.east ) +
                ( List.length state.west ) in
    let result = sprintf "|%d\t%Ld\t%Ld\t%Ld\t%d\n"
        nodes
        (to_msecs state.preproc)
        (to_msecs (snd gen_time)) (to_secs (snd synth_time))
        num_flows in
    Ok result
  | Some (edge, _) -> Ok (string_of_policy edge)


let coroway (policy,pedge) (fabric,fedge) net =
  let module T = Frenetic_Time in
  let module S = Frenetic_Synthesis.LP_Waypointing in
  let module A = Fabric.Assemblage in
  let state = Coronet.state in
  let result = Coronet.result in
  let topo = (CoroNet.Pretty.to_netkat net) in
  let fabric = A.assemble fabric topo fedge fedge in
  let policy = A.assemble policy topo pedge pedge in

  let pts, ptmsg = match state.waypoints with
    | Some i -> i, sprintf "Using %d waypoints" i
    | None -> 2, sprintf "No waypoints specified, using 2" in

  try
    let start = T.time () in
    let pol = A.to_dyads policy in
    let pol' = List.map pol ~f:(fun dyad ->
        let src,_ = Fabric.Dyad.src dyad in
        let dst,_ = Fabric.Dyad.dst dyad in
        let paths = List.permute
            (Hashtbl.Poly.find_exn Coronet.state.pointtbl (src,dst)) in
        let points = match List.findi paths ~f:(fun _ p -> List.length p >= pts) with
          | Some (_,p) -> fst( List.split_n p pts)
          | None -> raise (ShortPath (sprintf "Can't find path with %d points" pts)) in
        dyad,points) in
    let pol_time = ("Policy time", T.from start) in

    let start = T.time () in
    let fab = A.to_dyads fabric in
    let fab' = List.map fab ~f:(fun dyad ->
        let src = Fabric.Dyad.src dyad in
        let dst = Fabric.Dyad.dst dyad in
        let path = Hashtbl.Poly.find_exn Coronet.state.pathtbl (src,dst) in
        dyad,path ) in
    let fab_time = ("Fabric time", T.from start) in

    let edge, results = S.synthesize pol' fab' topo in
    log "Synthesized edge policy:\n%s\n%!" (string_of_policy edge);

    let nodes = ( List.length state.east ) +
                ( List.length state.west ) in
    let report = result nodes (pol_time::fab_time::results) in
    let edge' = string_of_policy edge in
    let msg = String.concat ~sep:"\n"
        ("Edge policies compiled successfully" :: edge':: report ) in
    Ok msg
  with
  | ShortPath e ->
    Error e
  | Frenetic_LP.LPParseError e ->
    Error (sprintf "Cannot parse LP Solution: %s\n" e)


let corosynth (policy,pedge) (fabric,fedge) net cs =
  let module T = Frenetic_Time in
  let (>>=) = Result.(>>=) in
  let state = Coronet.state in
  let result = Coronet.result in
  let open Frenetic_Synthesis in
  let module A = Fabric.Assemblage in
  let topo = (CoroNet.Pretty.to_netkat net) in
  let fabric = A.assemble fabric topo fedge fedge in
  let policy = A.assemble policy topo pedge pedge in

  begin match cs with
    | SNative  -> Ok ( module MakeStrict(Optical) : DYADIC )
    | SOptical -> Ok ( module MakeStrict(Optical) : DYADIC )
    | SLP_E -> Ok ( module LP_Endpoints : DYADIC )
    | SSAT_E -> Ok ( module SAT_Endpoints : DYADIC )
    | _ -> Error "Coronet synthesis only works with LP and SATE"
  end >>= fun s ->

  let module S = (val s) in
  try
    let start = T.time () in
    let pol = A.to_dyads policy in
    let pol_time = ("Policy time", T.from start) in

    let start = T.time () in
    let fab = A.to_dyads fabric in
    let fab_time = ("Fabric time", T.from start) in

    let edge, timings = S.synthesize pol fab topo in

    log "Pre-synthesis user policy:\n%s\n%!" (string_of_policy (A.program policy));
    log "Synthesized edge policy:\n%s\n%!"   (string_of_policy edge);
    (* let edge_fdd = Compiler.compile_local ~options:keep_cache edge in *)
    (* state.edge <- Some { new_config with *)
    (*                      policy = edge; *)
    (*                      ingresses = c.ingresses; egresses = c.egresses; *)
    (*                      fdd = Some edge_fdd }; *)
    let nodes = ( List.length state.east ) +
                ( List.length state.west ) in
    let report = result nodes (pol_time::fab_time::timings) in
    let edge' = string_of_policy edge in
    let msg = String.concat ~sep:"\n" ("Edge policies compiled successfully" :: report ) in
    Ok msg
  with
  | Frenetic_LP.LPParseError e ->
    Error (sprintf "Cannot parse LP Solution: %s\n" e)

let rec coronet c =
  let open Coronet in
  match c with
  (* TODO(basus): Allow coronet topologies to be loaded from strings as well *)
  | CLoadTopo ( String s ) -> Error "Coronet topologies must be loaded from files"
  | CLoadTopo ( Filename fn ) ->
    let net,name_tbl,port_tbl = CoroNet.from_csv_file fn in
    state.network <- Some net; state.names <- name_tbl;
    state.ports <- port_tbl;
    let result = sprintf "Successfully loaded topology from %s\n" fn in
    Ok result

  | CLoadPaths (String s ) -> Error "Coronet path list must be loaded from files"
  | CLoadPaths ( Filename fn ) -> begin match state.network with
      | None -> Error "Loading a path file requires loading a Coronet topology first"
      | Some net ->
        let channel = In_channel.create fn in
        let paths = In_channel.fold_lines channel ~init:[] ~f:(fun acc line ->
            let stops = String.split ~on:';' line in
            let vertexes = List.map stops ~f:(fun n ->
                try
                  let name = String.strip n in
                  let label = Hashtbl.find_exn state.names name in
                  CoroNet.Topology.vertex_of_label net label
                with Not_found -> failwith (sprintf "No vertex named (%s)%!" n))
            in
            let path = CoroNet.CoroPath.from_vertexes net vertexes in
            path::acc) in
        state.paths <- paths;
        Ok "Loaded paths"
    end

  | CEast e    -> state.east <- e; Ok "East nodes loaded"
  | CWest w    -> state.west <- w; Ok "West nodes loaded"
  | CClasses i ->
    state.classes <- Some i;
    Ok (sprintf "Ready to generate %d traffic classes" i)
  | CWaypoints i ->
    state.waypoints <- Some i;
    Ok (sprintf "Requiring %d waypoints per path" i)

  | CPreprocess ->  begin match state.network with
      | None ->
        Error "Coronet synthesis requires loading a Coronet topology first"
      | Some net -> try
          let open Frenetic_Time in
          let open Frenetic_NetKAT in

          let names, ports, east, west, paths =
            ( state.names, state.ports, state.east, state.west, state.paths ) in
          (* Attach hosts and packet switches to the optical switches *)
          printf "Generating topology\n%!";
          let net, ids, fedge, pedge = CoroNet.surround net names ports east west paths in
          state.network <- Some net;
          state.switches <- ids;

          (* Connect bicoastal pairs of nodes using the specified paths *)
          printf "Generating waypointed paths\n%!";
          let waypaths,wptbl = CoroNet.Waypath.of_coronet net names ports east west paths in

          (* Generate the optical fabric based on the given paths. Also the
             record the switches making up each path for waypointing. *)
          printf "Generating optical circuits\n%!";
          let circuits = List.map waypaths ~f:(fun wp ->
              let open CoroNet.Waypath in
              let _,ss,sp = wp.start in
              let _,ds,dp = wp.stop  in
              let points = List.map wp.waypoints
                  ~f:(CoroNet.Topology.vertex_to_id net) in
              Hashtbl.Poly.add_exn state.pathtbl ((ss,sp),(ds,dp)) points;
              to_circuit wp net ) in

          match Frenetic_Circuit_NetKAT.validate_config circuits with
          | Error e -> Error e
          | Ok circuits ->
            printf "Generating fabric program\n%!";
            let fab = Frenetic_Circuit_NetKAT.local_policy_of_config circuits in
            state.fabric <- Some (fab, fedge);

            (* Generate user policies, either using the number of given traffic
               classes, or the number of phsyical paths. *)
            let preds = match state.classes with
              | Some c -> CoroNet.predicates c
              | None   -> CoroNet.predicates (List.length state.paths) in

            printf "Generating policy program\n%!";
            let pols, tbl = CoroNet.Waypath.to_policies wptbl net preds in
            let pol = Frenetic_NetKAT_Optimize.mk_big_union pols in
            state.pointtbl <- tbl;
            state.policy <- Some (pol, pedge);

            (* Generate the fibers to be passed to the SMT synthesis backend *)
            (* let start = time () in *)
            (* printf "Generating fibers\n%!"; *)
            (* let fsfabric = CoroNet.Waypath.to_fabric_fibers waypaths net in *)
            (* let fspolicy = CoroNet.Waypath.to_policy_fibers wptbl net preds in *)
            (* state.preproc <- from start; *)
            (* state.fibers  <- (fspolicy, fsfabric); *)

            Ok "Coronet experiment preprocessed"
        with
        | CoroNet.NonexistentNode s ->
          Error (sprintf "No node %s in topology\n" s)
        | CoroNet.CoroPath.UnjoinablePaths s ->
          Error (sprintf "Unjoinable paths %s\n" s) end

  | CPath(s, d) -> begin match state.network with
      | None -> Error "Finding a path requires loading a Coronet topology"
      | Some net ->
        let open CoroNet in
        let names = state.names in
        let src = Hashtbl.Poly.find_exn names s in
        let dst = Hashtbl.Poly.find_exn names d in
        let src' = Topology.vertex_of_label net src in
        let dst' = Topology.vertex_of_label net dst in
        match CoroPath.shortest_path net src' dst' with
        | Some p ->
          state.paths <- p::state.paths;
          Ok (sprintf "Path from %s to %s: [%s]\n" s d ( CoroPath.to_string p) )
        | None -> Error (sprintf "No path between %s and %s" s d )
    end
  | CSMT -> begin match state.fibers, state.network with
      | _, None
      | ([], []), _ ->
        Error "Load and preprocess Coronet topologies and paths first"
      | (fspolicy, fsfabric), Some net ->
        (* Call the synthesis back-end *)
        let module C = Frenetic_Synthesis.Coronet in
        let netkat = (CoroNet.Pretty.to_netkat net) in
        let results = C.synthesize fspolicy fsfabric netkat in
        state.result <- results;

        (* Register a callback to print the results when they're ready *)
        Deferred.upon results (fun r ->
            printf "Coronet Synthesis complete";
            match cpeek () with
            | Ok result -> printf "%s\n" result
            | Error e -> printf "Error:%s\n" e
            (* The shell could be terminated at this point, if being run in
               batch mode to gather experimental numbers. *)
          );
        Ok "Coronet Solution started" end

  | CPeek -> cpeek ()
  | CSynthesize cs -> begin match state.policy, state.fabric, state.network with
      | _, None, _
      | None, _, _
      | _, _, None ->
        Error "Load and preprocess Coronet topologies and paths first"
      | Some p, Some f, Some net ->
        try ( match cs with
            | SLP_W -> coroway p f net
            | _ -> corosynth p f net cs )
        with
        | Frenetic_Synthesis.UnmatchedDyad d ->
          let fabric = string_of_policy (fst f) in
          let net' = sprintf "Surrounded topology:\n%s\n%!"
              (CoroNet.Pretty.to_mininet net) in
          let msg = sprintf "Unmatched dyad:%s\n" (Frenetic_Fabric.Dyad.to_string d) in
          Error ( fabric ^ net' ^ msg)
    end

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
        let fabric      = Fabric.Assemblage.assemble fpol topo fins fouts in
        let fab_streams = Fabric.Assemblage.to_dyads fabric in
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
  | Coronet c ->
    let r = coronet c in
    ( match r with
      | Ok msg  -> printf "%s\n" msg
      | Error e -> printf "Error: %s\n" e)
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

let () : unit =
  Gc.set { (Gc.get()) with
    Gc.Control.space_overhead = 500;
    Gc.Control.minor_heap_size = 32 * 1024 * 1024;
    Gc.Control.major_heap_increment = 256 * 1024 * 1024;
    Gc.Control.max_overhead = 700;
  };

  Async_parallel_deprecated.Std.Parallel.init ();
  Log.set_output [Async.Std.Log.Output.file `Text log_filename];
  printf "Frenetic PowerShell v 1.0\n%!";
  printf "Type `help` for a list of commands\n%!";
  ignore(repl ());
  never_returns (Async.Std.Scheduler.go ())
