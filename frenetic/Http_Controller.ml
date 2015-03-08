open Core.Std
open Async.Std
open Cohttp_async
open NetKAT_Types
open Common
module Server = Cohttp_async.Server
module Log = Async_OpenFlow.Log

type client = {
  (* Write new policies to this node *)
  policy_node: (DynGraph.cannot_receive, policy) DynGraph.t;
  (* Read from this pipe to send events *)
  event_reader: string Pipe.Reader.t;
  (* Write to this pipe when new event received from the network *)
  event_writer: string Pipe.Writer.t;
}

let unions (pols : policy list) : policy =
  List.fold_left pols ~init:drop ~f:(fun p q -> Union (p, q))

let pol : (policy, policy) DynGraph.t = DynGraph.create drop unions

let clients : (string, client) Hashtbl.t = Hashtbl.Poly.create ()

let iter_clients (f : string -> client -> unit) : unit =
  Hashtbl.iter clients ~f:(fun ~key ~data -> f key data)

let rec propogate_events event =
  event () >>=
  fun evt ->
  let response = NetKAT_Json.event_to_json_string evt in
  (* TODO(jcollard): Is there a mapM equivalent here? *)
  Hashtbl.iter clients (fun ~key ~data:client ->
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
               { policy_node = node; event_reader = r; event_writer =  w })

let handle_request
  (module Controller : NetKAT_Controller.CONTROLLER)
  ~(body : Cohttp_async.Body.t)
  (client_addr : Socket.Address.Inet.t)
  (request : Request.t) : Server.response Deferred.t =
  let open Controller in
  Log.info "%s %s" (Cohttp.Code.string_of_method request.meth)
    (Uri.path request.uri);
  match request.meth, extract_path request with
    | `GET, ["query"; name] ->
      if (is_query name) then
        query name
        >>= fun stats ->
        Server.respond_with_string (NetKAT_Json.stats_to_json_string stats)
      else
        begin
          Log.info "query %s is not defined in the current policy" name;
          let headers = Cohttp.Header.init_with "X-Query-Not-Defined" "true" in
          Server.respond_with_string ~headers
            (NetKAT_Json.stats_to_json_string (0L, 0L))
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
           NetKAT_SDN_Json.pkt_out_from_json json)
        (fun (sw_id, pkt_out) ->
           send_packet_out sw_id pkt_out
           >>= fun () ->
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
    | _, _ ->
      Log.error "Unknown method/path (404 error)";
      Cohttp_async.Server.respond `Not_found

let print_error addr exn =
  Log.error "%s" (Exn.to_string exn)

let listen ~http_port ~openflow_port =
  Async_OpenFlow.OpenFlow0x01.Controller.create ~port:openflow_port ()
  >>= fun controller ->
  let module Controller = NetKAT_Controller.Make (struct
      let controller = controller
    end) in
  let on_handler_error = `Call print_error in
  let _ = Cohttp_async.Server.create
    ~on_handler_error
    (Tcp.on_port http_port)
    (handle_request (module Controller)) in
  let (_, pol_reader) = DynGraph.to_pipe pol in
  let _ = Pipe.iter pol_reader ~f:(fun pol -> Controller.update_policy pol) in
  Controller.start ();
  don't_wait_for(propogate_events Controller.event);
  Deferred.return ()


let main (http_port : int) (openflow_port : int) () : unit =
  don't_wait_for(listen ~http_port ~openflow_port)

