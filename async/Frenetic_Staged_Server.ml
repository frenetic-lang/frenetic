(* This implements a staged webserver interface to the virtual compiler. It is *)
(* based on the Compile_Server. It allows POSTing partial inputs of the *)
(* compile job separately, and then running the compile job, instead of *)
(* hardcoding them in. *)

open Core.Std
open Async.Std
open Cohttp_async
open Frenetic_NetKAT
open Frenetic_NetKAT_Pretty
module Server = Cohttp_async.Server
(* open Common *)

type vno = { id                : int;
             policy            : policy ;
             relation          : pred ;
             topology          : policy ;
             ingress_policy    : policy ;
             ingress_predicate : pred ;
             egress_predicate  : pred ;
           }

let default_vno = { id                = 0  ;
                    policy            = Filter True ;
                    relation          = True ;
                    topology          = Filter True ;
                    ingress_policy    = Filter True ;
                    ingress_predicate = True ;
                    egress_predicate  = True ;
                  }

let topology          = ref (Filter True)
let ingress_predicate = ref True
let egress_predicate  = ref True
let compiled          = ref None
let compiler_options  = ref Frenetic_NetKAT_Compiler.default_compiler_options

let vnos = Hashtbl.create ~hashable:Int.hashable ()

let parse_selection (string:string) : int list =
  let string_ids = String.split string ~on:':' in
  List.map string_ids ~f:int_of_string

let parse_remove (string:string) : bool =
  (String.lowercase string) = "true"

let parse_pol = Frenetic_NetKAT_Parser.policy_from_string
let parse_pol_json = Frenetic_NetKAT_Json.policy_from_json_string
let parse_pred = Frenetic_NetKAT_Parser.pred_from_string
let respond = Cohttp_async.Server.respond_with_string

let respond_unknowns (unknowns:int list) : string =
  let buffer = Bigbuffer.create (List.length unknowns) in
  Bigbuffer.add_string buffer "Unknown VNOs: ";
  List.iter unknowns ~f:(fun id ->
    Bigbuffer.add_string buffer (string_of_int id);
    Bigbuffer.add_string buffer ";");
  Bigbuffer.contents buffer

let rec virtualize_pred pred =
  match pred with
  | True -> True
  | False -> False
  | Test (Switch sw) -> Test (VSwitch sw)
  | Test (Location (Physical pt)) -> Test (VPort (Int64.of_int32 pt))
  | Test hv -> Test hv
  | And (a, b) -> And (virtualize_pred a, virtualize_pred b)
  | Or (a, b) -> Or (virtualize_pred a, virtualize_pred b)
  | Neg a -> Neg (virtualize_pred a)

let rec virtualize_pol pol =
  match pol with
  | Filter pred -> Filter (virtualize_pred pred)
  | Mod (Location (Physical pt)) -> Mod (VPort (Int64.of_int32 pt))
  | Union (p, q) -> Union (virtualize_pol p, virtualize_pol q)
  | Seq (p, q) -> Seq (virtualize_pol p, virtualize_pol q)
  | Star p -> Star (virtualize_pol p)
  | _ -> assert false

type stage =
  | VAdd              of int
  | VRemove           of int
  | VPolicy           of int
  | VRelation         of int
  | VTopology         of int
  | VIngressPolicy    of int
  | VIngressPredicate of int
  | VEgressPredicate  of int
  | PTopology
  | PIngressPredicate
  | PEgressPredicate
  | Compile
  | CompileLocal      of switchId
  | CompileSelective
  | FlowTable         of switchId
  | RemoveDuplicates    of bool
  | Unknown

let request_to_stage (req : Request.t) : stage =
  let parts = List.filter ~f:(fun str -> not (String.is_empty str))
    (String.split ~on:'/'
       (Uri.path req.uri)) in
  match parts with
  | [ "add-vno"; i ]                      -> VAdd (int_of_string i)
  | [ "remove-vno"; i ]                   -> VRemove (int_of_string i)
  | [ "virtual-policy"; vno ]             -> VPolicy (int_of_string vno)
  | [ "virtual-relation"; vno ]           -> VRelation (int_of_string vno)
  | [ "virtual-topology" ; vno ]          -> VTopology (int_of_string vno)
  | [ "virtual-ingress-policy" ; vno ]    -> VIngressPolicy (int_of_string vno)
  | [ "virtual-ingress-predicate" ; vno ] -> VIngressPredicate (int_of_string vno)
  | [ "virtual-egress-predicate" ; vno ]  -> VEgressPredicate (int_of_string vno)
  | [ "physical-topology" ]               -> PTopology
  | [ "physical-ingress-predicate" ]      -> PIngressPredicate
  | [ "physical-egress-predicate" ]       -> PEgressPredicate
  | [ "compile" ]                         -> Compile
  | [ "compile-local" ; sw ]              -> CompileLocal (Int64.of_string sw)
  | [ "compile-selective" ]               -> CompileSelective
  | [ "get-flowtable" ; sw]               -> FlowTable (Int64.of_string sw)
  | [ "remove-duplicates" ; s ]           -> RemoveDuplicates (parse_remove s)
  | _                                     -> Unknown


let attempt_vno_update i body parse update default =
  (Body.to_string body) >>= (fun s ->
    let value = parse s in
    match Hashtbl.find vnos i with
    | Some vno -> Hashtbl.replace vnos i (update vno value) ; respond "Replace"
    | None -> Hashtbl.add_exn vnos i (default value) ; respond "OK")

let attempt_phys_update body parse loc =
  (Body.to_string body) >>= (fun s ->
    try
      let value = parse s in loc := value;
      respond "Replace"
    with
    | _ -> respond "Parse error")

let compile vno =
  print_endline "VNO Policy";
  print_endline (string_of_policy vno.policy);
  print_endline "VNO Ingress Policy";
  print_endline (string_of_policy vno.ingress_policy);
  print_endline "VNO Topology";
  print_endline (string_of_policy vno.topology);
  print_endline "VNO Relation";
  print_endline (string_of_pred vno.relation);
  print_endline "VNO Ingress predicate";
  print_endline (string_of_pred vno.ingress_predicate);
  print_endline "Physical Topology";
  print_endline  (string_of_policy !topology);
  print_endline "Physical ingress predicate";
  print_endline (string_of_pred !ingress_predicate);
  print_endline "Physical egress predicate";
  print_endline (string_of_pred !egress_predicate);
  (Frenetic_NetKAT_Virtual_Compiler.compile vno.policy
     vno.relation vno.topology vno.ingress_policy
     vno.ingress_predicate vno.egress_predicate
     !topology !ingress_predicate
     !egress_predicate)

let global_compile (policy : policy)  : unit =
  compiled := Some( Frenetic_NetKAT_Compiler.compile_global policy )

let compile_vnos vno_list =
  match List.hd vno_list, List.tl vno_list with
  | Some hd, Some tl ->
    let union = List.fold (List.tl_exn vno_list)
      ~init:(compile (List.hd_exn vno_list))
      ~f:(fun acc vno -> Frenetic_NetKAT_Optimize.mk_union acc (compile vno)) in
    global_compile union;
    "OK"
  | Some hd, None ->
    global_compile (compile hd) ;
    "OK"
  | _ ->
    compiled := None;
    "None"

let handle_request
    ~(body : Cohttp_async.Body.t)
    (client_addr : Socket.Address.Inet.t)
    (request : Request.t) : Server.response Deferred.t =
  match request.meth, request_to_stage request with
  | `GET, VAdd i -> begin
    match Hashtbl.add vnos i {default_vno with id = i} with
    | `Duplicate -> respond "Replace"
    | `Ok -> respond "OK" end
  | `GET, VRemove i ->
    Hashtbl.remove vnos i ;
    respond "OK"
  | `POST, VPolicy i ->
    attempt_vno_update i body
      (fun s -> virtualize_pol (parse_pol_json s))
      (fun v p -> {v with policy = p})
      (fun p -> {default_vno with policy = p})
  | `POST, VRelation i ->
    attempt_vno_update i body parse_pred (fun v r -> {v with relation = r})
      (fun r -> {default_vno with relation = r})
  | `POST, VTopology i ->
    attempt_vno_update i body parse_pol (fun v t -> {v with topology = t})
      (fun t -> {default_vno with topology = t})
  | `POST, VIngressPolicy i ->
    attempt_vno_update i body parse_pol_json (fun v p -> {v with ingress_policy = p})
      (fun p -> {default_vno with ingress_policy = p})
  | `POST, VIngressPredicate i ->
    attempt_vno_update i body parse_pred (fun v p -> {v with ingress_predicate = p})
      (fun p -> {default_vno with ingress_predicate = p})
  | `POST, VEgressPredicate i ->
    attempt_vno_update i body parse_pred (fun v p -> {v with egress_predicate = p})
      (fun p -> {default_vno with egress_predicate = p})
  | `POST, PTopology ->
    attempt_phys_update body parse_pol topology
  | `POST, PIngressPredicate ->
    attempt_phys_update body parse_pred ingress_predicate
  | `POST, PEgressPredicate ->
    attempt_phys_update body parse_pred egress_predicate
  | `GET, Compile ->
    let vno_list = Hashtbl.fold vnos ~init:[]
      ~f:(fun ~key:id ~data:vno acc -> vno::acc) in
    respond (compile_vnos vno_list)
  | `POST, CompileLocal sw ->
    (Body.to_string body) >>= (fun s ->
      try
        parse_pol_json s |>
        (fun s -> print_endline "Parsing policy :";
                  print_endline (string_of_policy s);
                  s) |>
        Frenetic_NetKAT_Compiler.compile |>
        Frenetic_NetKAT_Compiler.to_table sw |>
        Frenetic_NetKAT_SDN_Json.flowTable_to_json |>
        Yojson.Basic.to_string ~std:true |>
        (fun s -> print_endline "Generated flowtable";
                  print_endline s;
                  s ) |>
        Cohttp_async.Server.respond_with_string
      with
      | Invalid_argument s ->
        print_endline s ; respond "Parse error"
      | Not_found ->
        respond "Parse error" )
  | `POST, CompileSelective ->
    (Body.to_string body) >>= (fun s ->
      let vno_ids = parse_selection s in
      let (vno_list, unknowns) = List.fold_left vno_ids ~init:([], [])
        ~f:(fun (acc, unknowns) id -> match Hashtbl.find vnos id with
        | Some vno -> (vno::acc, unknowns)
        | None -> (acc, id::unknowns)) in
      if (List.length unknowns > 0)
      then respond (respond_unknowns unknowns)
      else respond (compile_vnos vno_list))
  | `GET, FlowTable sw -> begin
    match !compiled with
    | None -> respond "None"
    | Some repr -> repr |>
        Frenetic_NetKAT_Compiler.to_table sw |>
        Frenetic_NetKAT_SDN_Json.flowTable_to_json |>
        Yojson.Basic.to_string ~std:true |>
        Cohttp_async.Server.respond_with_string end
  | `GET, RemoveDuplicates b ->
    compiler_options := { !compiler_options with dedup_flows = b }; respond "Updated"
  | _ -> respond "Unknown"


let main (port:int) () =
  print_endline "NetKAT compiler running";
  Frenetic_Fdd.Field.set_order
   [ Switch; Location; VSwitch; VPort; IP4Dst; Vlan; TCPSrcPort; TCPDstPort; IP4Src;
      EthType; EthDst; EthSrc; VlanPcp; IPProto ; VFabric; Wavelength];
  ignore (Cohttp_async.Server.create (Tcp.on_port port) handle_request)

(* let main (args : string list) : unit = match args with *)
(*   | [ "--port"; p ] | [ "-p"; p ] -> *)
(*     listen ~port:(Int.of_string p) () *)
(*   | [] -> listen ~port:9000 () *)
(*   |  _ -> (print_endline "Invalid command-line arguments"; Shutdown.shutdown 1) *)
