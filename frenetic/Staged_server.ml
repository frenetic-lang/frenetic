(** This implements a staged webserver interface to the virtual compiler. It is
    based on the Compile_Server. It allows POSTing partial inputs of the
    compile job separately, and then running the compile job, instead of
    hardcoding them in.
*)

open Core
open Async
open Cohttp_async
module Server = Cohttp_async.Server
module Netkat = Frenetic.Netkat
open Netkat.Syntax
module Virtual_compiler = Netkat.Virtual_Compiler.Make(Netkat.FabricGen.FabricGen)
module Global_compiler = Netkat.Global_compiler
module Local_compiler = Netkat.Local_compiler
module Field = Local_compiler.Field

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

let vnos = Hashtbl.create ~hashable:Int.hashable ()

let parse_selection (string:string) : int list =
  let string_ids = String.split string ~on:':' in
  List.map string_ids ~f:int_of_string

let parse_pol = Netkat.Parser.pol_of_string
let parse_pred = Netkat.Parser.pred_of_string

let respond = Cohttp_async.Server.respond_string

let respond_unknowns (unknowns:int list) : string =
  let buffer = Bigbuffer.create (List.length unknowns) in
  Bigbuffer.add_string buffer "Unknown VNOs: ";
  List.iter unknowns ~f:(fun id ->
    Bigbuffer.add_string buffer (string_of_int id);
    Bigbuffer.add_string buffer ";");
  Bigbuffer.contents buffer

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
  | Unknown

let request_to_stage (req : Request.t) : stage =
  let parts = List.filter ~f:(fun str -> not (String.is_empty str))
    (String.split ~on:'/' (Uri.path (Request.uri req))) in
  match parts with
  | [ "compile-local" ; sw ]              -> CompileLocal (Int64.of_string sw)
  | [ "virtual-policy"; vno ]             -> VPolicy (int_of_string vno)
  | [ "virtual-topology" ; vno ]          -> VTopology (int_of_string vno)
  | [ "virtual-ingress-policy" ; vno ]    -> VIngressPolicy (int_of_string vno)
  | [ "virtual-ingress-predicate" ; vno ] -> VIngressPredicate (int_of_string vno)
  | [ "virtual-egress-predicate" ; vno ]  -> VEgressPredicate (int_of_string vno)
  | [ "virtual-relation"; vno ]           -> VRelation (int_of_string vno)
  | [ "physical-topology" ]               -> PTopology
  | [ "physical-ingress-predicate" ]      -> PIngressPredicate
  | [ "physical-egress-predicate" ]       -> PEgressPredicate
  | [ "compile" ]                         -> Compile
  | [ "get-flowtable" ; sw]               -> FlowTable (Int64.of_string sw)

  | [ "add-vno"; i ]                      -> VAdd (int_of_string i)
  | [ "remove-vno"; i ]                   -> VRemove (int_of_string i)
  | [ "compile-selective" ]               -> CompileSelective
  | _                                     -> Unknown


let attempt_vno_update i body parse update default =
  (Body.to_string body) >>= (fun s ->
    let value = parse s in
    match Hashtbl.find vnos i with
    | Some vno -> Hashtbl.set vnos i (update vno value) ; respond "Replace"
    | None -> Hashtbl.add_exn vnos i (default value) ; respond "OK")

let attempt_phys_update body parse loc =
  (Body.to_string body) >>= (fun s ->
    try
      let value = parse s in loc := value;
      respond "Replace"
    with
    | _ -> respond "Parse error")

let compile vno =
  let open Netkat.Pretty in
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
  Virtual_compiler.compile
    ~vrel:vno.relation
    ~vtopo:vno.topology
    ~ving_pol:vno.ingress_policy
    ~ving:vno.ingress_predicate
    ~veg:vno.egress_predicate
    ~ptopo:(!topology)
    ~ping:(!ingress_predicate)
    ~peg:(!egress_predicate)
    vno.policy

let compile_vnos vno_list =
  let open Netkat.Optimize in
  if List.is_empty vno_list then begin
    compiled := None;
    "None"
  end else begin
    let global_pol =
      List.map vno_list ~f:compile
      |> List.fold ~init:drop ~f:mk_union
    in
    let local_pol = Global_compiler.compile 
      ~ing:(!ingress_predicate)
      ~pc:Field.Vlan
      global_pol 
    in
    compiled := Some (local_pol);
    "OK"
  end

let handle_request
    ~(body : Cohttp_async.Body.t)
    (client_addr : Socket.Address.Inet.t)
    (request : Request.t) : Server.response Deferred.t =
  let open Netkat.Pretty in
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
      (fun s -> Netkat.Syntax.virtualize_pol (parse_pol s))
      (fun v p -> {v with policy = p})
      (fun p -> {default_vno with policy = p})
  | `POST, VRelation i ->
    attempt_vno_update i body parse_pred (fun v r -> {v with relation = r})
      (fun r -> {default_vno with relation = r})
  | `POST, VTopology i ->
    attempt_vno_update i body parse_pol (fun v t -> {v with topology = t})
      (fun t -> {default_vno with topology = t})
  | `POST, VIngressPolicy i ->
    attempt_vno_update i body parse_pol (fun v p -> {v with ingress_policy = p})
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
        parse_pol s |>
        (fun s -> print_endline "Parsing policy :";
                  print_endline (string_of_policy s);
                  s) |>
        Local_compiler.compile |>
        Local_compiler.to_table sw |>
        Netkat.Json.flowTable_to_json |>
        Yojson.Basic.to_string ~std:true |>
        (fun s -> print_endline "Generated flowtable";
                  print_endline s;
                  s ) |>
        Cohttp_async.Server.respond_string
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
  | `GET, FlowTable sw ->
    begin match !compiled with
    | None -> respond "None"
    | Some local ->
       Local_compiler.to_table sw local
       |> Netkat.Json.flowTable_to_json
       |> Yojson.Basic.to_string ~std:true 
       |> Cohttp_async.Server.respond_string
    end
  | _ -> respond "Unknown"


let listen ?(port=9000) () =
  print_endline "NetKAT compiler running";
  ignore (Cohttp_async.Server.create (Tcp.on_port port) handle_request)

let main (args : string list) : unit = match args with
  | [ "--port"; p ] | [ "-p"; p ] ->
    listen ~port:(Int.of_string p) ()
  | [] -> listen ~port:9000 ()
  |  _ -> (print_endline "Invalid command-line arguments"; Shutdown.shutdown 1)
