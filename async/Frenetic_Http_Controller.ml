open Core.Std
open Async.Std
open Cohttp_async
open Frenetic_NetKAT
open Frenetic_Common
module Server = Cohttp_async.Server
module Comp = Frenetic_NetKAT_Compiler
module Log = Frenetic_Log

type client = {
  (* Write new policies to this node *)
  policy_node: (Frenetic_DynGraph.cannot_receive, policy) Frenetic_DynGraph.t;
  (* Read from this pipe to send events *)
  event_reader: string Pipe.Reader.t;
  (* Write to this pipe when new event received from the network *)
  event_writer: string Pipe.Writer.t;
}

let current_compiler_options = ref Comp.default_compiler_options

(* TODO(arjun):

  <facepalm>

  These are OpenFlow 1.0 types. Everywhere else, we are using SDN_Types. *)
let port_to_json port = `Int (Int32.to_int_exn port)

let switch_and_ports_to_json (sw, ports) =
  `Assoc [("switch_id", `Int (Int64.to_int_exn sw));
          ("ports", `List (List.map ~f:port_to_json ports))]

let current_switches_to_json lst =
  `List (List.map ~f:switch_and_ports_to_json lst)

let current_switches_to_json_string lst =
  Yojson.Basic.to_string ~std:true (current_switches_to_json lst)
(* </facepalm> *)

let unions (pols : policy list) : policy =
  List.fold_left pols ~init:drop ~f:(fun p q -> Union (p, q))

let pol : (policy, policy) Frenetic_DynGraph.t = Frenetic_DynGraph.create drop unions

let clients : (string, client) Hashtbl.t = Hashtbl.Poly.create ()

let iter_clients (f : string -> client -> unit) : unit =
  Hashtbl.iter clients ~f:(fun ~key ~data -> f key data)

let rec propogate_events event =
  event () >>=
  fun evt ->
  let response = Frenetic_NetKAT_Json.event_to_json_string evt in
  (* TODO(jcollard): Is there a mapM equivalent here? *)
  Hashtbl.iter clients (fun ~key ~data:client ->
    Pipe.write_without_pushback client.event_writer response);
  propogate_events event

(* Gets the client's node in the dataflow graph, or creates it if doesn't exist *)
let get_client (clientId: string): client =
  Hashtbl.find_or_add clients clientId
     ~default:(fun () ->
               printf ~level:`Info "New client %s" clientId;
               let node = Frenetic_DynGraph.create_source drop in
               Frenetic_DynGraph.attach node pol;
	       let (r, w) = Pipe.create () in
               { policy_node = node; event_reader = r; event_writer =  w })

let handle_request
  (module Controller : Frenetic_NetKAT_Controller.CONTROLLER)
  ~(body : Cohttp_async.Body.t)
  (client_addr : Socket.Address.Inet.t)
  (request : Request.t) : Server.response Deferred.t =
  let open Controller in
  Log.info "%s %s" (Cohttp.Code.string_of_method request.meth)
    (Uri.path request.uri);
  match request.meth, extract_path request with
    | `GET, ["version"] -> Server.respond_with_string "4"
    | `GET, ["port_stats"; switch_id; port_id] ->
       port_stats (Int64.of_string switch_id) (Int32.of_string port_id)
       >>= fun portStats ->
       Server.respond_with_string (Frenetic_NetKAT_Json.port_stats_to_json_string portStats)
    | `GET, ["current_switches"] ->
      current_switches () >>= fun switches ->
      Server.respond_with_string (current_switches_to_json_string switches)
    | `GET, ["query"; name] ->
      if (is_query name) then
        query name
        >>= fun stats ->
        Server.respond_with_string (Frenetic_NetKAT_Json.stats_to_json_string stats)
      else
        begin
          Log.info "query %s is not defined in the current policy" name;
          let headers = Cohttp.Header.init_with "X-Query-Not-Defined" "true" in
          Server.respond_with_string ~headers
            (Frenetic_NetKAT_Json.stats_to_json_string (0L, 0L))
        end
    | `GET, [clientId; "event"] ->
      let curr_client = get_client clientId in
      (* Check if there are events that this client has not seen yet *)
      Pipe.read curr_client.event_reader
      >>= (function
      | `Eof -> assert false
      | `Ok response -> Server.respond_with_string response)
    | `POST, ["pkt_out"] ->
      handle_parse_errors' body
        (fun str ->
           let json = Yojson.Basic.from_string str in
           Frenetic_NetKAT_SDN_Json.pkt_out_from_json json)
        (fun (sw_id, pkt_out) ->
           send_packet_out sw_id pkt_out
           >>= fun () ->
           Cohttp_async.Server.respond `OK)
    | `POST, [clientId; "update_json"] ->
      handle_parse_errors body parse_update_json
      (fun pol ->
         Frenetic_DynGraph.push pol (get_client clientId).policy_node;
         Cohttp_async.Server.respond `OK)
    | `POST, [clientId; "update" ] ->
      handle_parse_errors body parse_update
      (fun pol ->
         Frenetic_DynGraph.push pol (get_client clientId).policy_node;
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
       Cohttp_async.Server.respond_with_string
     | _, _ ->
      Log.error "Unknown method/path (404 error)";
      Cohttp_async.Server.respond `Not_found

let print_error addr exn =
  Log.error "%s" (Exn.to_string exn)

let listen ~http_port ~openflow_port =
  let module Controller = Frenetic_NetKAT_Controller.Make in
  let on_handler_error = `Call print_error in
  let _ = Cohttp_async.Server.create
    ~on_handler_error
    (Tcp.on_port http_port)
    (handle_request (module Controller)) in
  let (_, pol_reader) = Frenetic_DynGraph.to_pipe pol in
  let _ = Pipe.iter pol_reader ~f:(fun pol -> Controller.update_policy pol) in
  Controller.start openflow_port;
  don't_wait_for(propogate_events Controller.event);
  Deferred.return ()


let main (http_port : int) (openflow_port : int) () : unit =
  don't_wait_for(listen ~http_port ~openflow_port)
