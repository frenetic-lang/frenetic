open Core
open Async
open Cohttp_async
open Frenetic_netkat.Syntax
open Common
module Server = Cohttp_async.Server
module Comp = Frenetic_netkat.Local_compiler

type client = {
  (* Write new policies to this node *)
  policy_node: (DynGraph.cannot_receive, policy) DynGraph.t;
  (* Read from this pipe to send events *)
  event_reader: string Pipe.Reader.t;
  (* Write to this pipe when new event received from the network *)
  event_writer: string Pipe.Writer.t;
}

let current_compiler_options = ref Comp.default_compiler_options

let port_to_json port = `Int (Int32.to_int_exn port)

let switch_and_ports_to_json (sw, ports) =
  `Assoc [("switch_id", `Int (Int64.to_int_exn sw));
          ("ports", `List (List.map ~f:port_to_json ports))]

let current_switches_to_json lst =
  `List (List.map ~f:switch_and_ports_to_json lst)

let current_switches_to_json_string lst =
  Yojson.Basic.to_string ~std:true (current_switches_to_json lst)

let unions (pols : policy list) : policy =
  List.fold_left pols ~init:drop ~f:(fun p q -> Union (p, q))

let pol : (policy, policy) DynGraph.t = DynGraph.create drop unions

let clients : (string, client) Hashtbl.t = Hashtbl.Poly.create ()

let iter_clients (f : string -> client -> unit) : unit =
  Hashtbl.iteri clients ~f:(fun ~key ~data -> f key data)

let rec propogate_events event =
  event () >>=
  fun evt ->
  let response = Frenetic_netkat.Json.event_to_json_string evt in
  (* TODO(jcollard): Is there a mapM equivalent here? *)
  Hashtbl.iteri clients (fun ~key ~data:client ->
    Pipe.write_without_pushback client.event_writer response);
  propogate_events event

(* Gets the client's node in the dataflow graph, or creates it if doesn't exist *)
let get_client (clientId: string): client =
  Hashtbl.find_or_add clients clientId
    ~default:(fun () ->
      printf ~level:`Info "New client %s" clientId;
      let node = DynGraph.create_source drop in
      DynGraph.attach node pol;
	    let (r, w) = Pipe.create () in
      { policy_node = node; event_reader = r; event_writer =  w }
    )

(* The Controller module is a parameter because port_stats and packet_out are called directly. *)
let handle_request
  (module Controller : NetKAT_Controller.CONTROLLER)
  ~(body : Cohttp_async.Body.t)
  (client_addr : Socket.Address.Inet.t)
  (request : Request.t) : Server.response Deferred.t =
  let open Controller in
  Logging.info "%s %s" (Cohttp.Code.string_of_method request.meth)
    (Uri.path (Request.uri request));
  match request.meth, extract_path request with
    | `GET, ["version"] -> Server.respond_string "4"
    | `GET, ["port_stats"; switch_id; port_id] ->
       port_stats (Int64.of_string switch_id) (Int32.of_string port_id)
       >>= fun portStats ->
       Server.respond_string (Frenetic_netkat.Json.port_stat_to_json_string portStats)
    | `GET, ["current_switches"] ->
      switches () >>= fun switches ->
      Server.respond_string (current_switches_to_json_string switches)
    | `GET, ["query"; name] ->
       (* TODO: check if query exists *)
       query name
       >>= fun stats ->
       Server.respond_string (Frenetic_netkat.Json.stats_to_json_string stats)
    (* begin *)
    (*   Logging.info "query %s is not defined in the current policy" name; *)
    (*   let headers = Cohttp.Header.init_with "X-Query-Not-Defined" "true" in *)
    (*   Server.respond_string ~headers *)
    (*     (Frenetic_netkat.Json.stats_to_json_string (0L, 0L)) *)
    (* end *)
    | `GET, [clientId; "event"] ->
      let curr_client = get_client clientId in
      (* Check if there are events that this client has not seen yet *)
      Pipe.read curr_client.event_reader
      >>= (function
      | `Eof -> assert false
      | `Ok response -> Server.respond_string response)
    | `POST, ["pkt_out"] ->
      handle_parse_errors' body
        (fun str ->
           let json = Yojson.Basic.from_string str in
           Frenetic_netkat.Json.pkt_out_from_json json)
        (fun (sw_id, port_id, payload, policies) ->
           packet_out sw_id port_id payload policies >>= fun () ->
           Cohttp_async.Server.respond `OK)
    | `POST, [clientId; "update_json"] ->
      handle_parse_errors body parse_update_json
      (fun pol ->
         DynGraph.push pol (get_client clientId).policy_node;
         Cohttp_async.Server.respond `OK)
    | `POST, [clientId; "update" ] ->
      handle_parse_errors body parse_update
      (fun pol ->
         DynGraph.push pol (get_client clientId).policy_node;
         Cohttp_async.Server.respond `OK)
    | `POST, ["config"] ->
       printf "POST /config";
       handle_parse_errors body parse_config_json
        (fun conf ->
          current_compiler_options := conf;
          set_current_compiler_options conf;
          Cohttp_async.Server.respond `OK)
    | `GET, ["config"] ->
       printf "GET /config";
       Comp.options_to_json_string !current_compiler_options |>
       Cohttp_async.Server.respond_string
     | _, _ ->
      Logging.error "Unknown method/path (404 error)";
      Cohttp_async.Server.respond `Not_found

let print_error addr exn =
  let monitor_exn = Exn.to_string (Monitor.extract_exn exn) in
  (* This is really kludgy, but the exception is of unknown type *)
  match String.substr_index monitor_exn ~pattern:"writer fd unexpectedly closed" with
  | Some _ ->  Logging.info "Ignoring writer exception"
  | None -> Logging.error "%s" monitor_exn

let listen ~http_port ~openflow_port =
  let module Controller = NetKAT_Controller.Make(OpenFlow0x01_Plugin) in
  let on_handler_error = `Call print_error in
  let _ = Cohttp_async.Server.create
    ~on_handler_error
    (Tcp.Where_to_listen.of_port http_port)
    (handle_request (module Controller)) in
  let (_, pol_reader) = DynGraph.to_pipe pol in
  let _ = Pipe.iter pol_reader ~f:(fun pol -> Controller.update pol) in
  Controller.start openflow_port;
  don't_wait_for(propogate_events Controller.event);
  Deferred.return ()


let main (http_port : int) (openflow_port : int) () : unit =
  don't_wait_for(listen ~http_port ~openflow_port)
