open Core.Std
open Async.Std
open Cohttp_async
open NetKAT_Types
open Common
module Server = Cohttp_async.Server

type client = {
  (* Write new policies to this node *)
  policy_node: (DynGraph.cannot_receive, policy) DynGraph.t;
  (* Read from this pipe to send events *)
  event_reader: event Pipe.Reader.t;
  (* Write to this pipe when new event received from the network *)
  event_writer: event Pipe.Writer.t
}

let unions (pols : policy list) : policy =
  List.fold_left pols ~init:drop ~f:(fun p q -> Union (p, q))

let pol : (policy, policy) DynGraph.t = DynGraph.create drop unions

let clients : (string, client) Hashtbl.t = Hashtbl.Poly.create ()

let iter_clients (f : string -> client -> unit) : unit =
  Hashtbl.iter clients ~f:(fun ~key ~data -> f key data)

(* Gets the client's node in the dataflow graph, or creates it if doesn't exist *)
let get_client (clientId: string): client =
  Hashtbl.find_or_add clients clientId
     ~default:(fun () ->
               printf ~level:`Info "New client %s" clientId;
               let node = DynGraph.create_source drop in
               DynGraph.attach node pol;
               let (r, w) = Pipe.create () in
               { policy_node = node; event_reader = r; event_writer =  w})

let handle_request
  (event : unit -> event Deferred.t)
  (send_packet_out : switchId -> SDN_Types.pktOut -> unit Deferred.t)
  (query : string -> (Int64.t * Int64.t) Deferred.t)
  ~(body : Cohttp_async.Body.t)
  (client_addr : Socket.Address.Inet.t)
  (request : Request.t) : Server.response Deferred.t =
  match request.meth, extract_path request with
    | `GET, ["query"; name] ->
      printf "GET /query/%s" name;
      query name
      >>= fun stats ->
      Server.respond_with_string (NetKAT_Json.stats_to_json_string stats)
    | `GET, [clientId; "event"] ->
      printf "GET /event";
      event ()
      >>= fun evt ->
      Server.respond_with_string (NetKAT_Json.event_to_json_string evt)
    | `POST, ["pkt_out"] ->
      handle_parse_errors' body
        (fun str ->
           let json = Yojson.Basic.from_string str in
           NetKAT_SDN_Json.pkt_out_from_json json)
        (fun (sw_id, pkt_out) ->
           printf "POST /pkt_out";
           send_packet_out sw_id pkt_out
           >>= fun () ->
           Cohttp_async.Server.respond `OK)
    | `POST, [clientId; "update_json"] ->
      Body.to_string body 
      >>= fun body_str ->  
      printf "POST /%s/update_json\n$$$%s$$$" clientId body_str;
      handle_parse_errors'' body_str NetKAT_Json.policy_from_json_string
      (fun pol ->
         DynGraph.push pol (get_client clientId).policy_node;
         Cohttp_async.Server.respond `OK)
    | `POST, [clientId; "update" ] ->
      printf "POST /%s/update" clientId;
      handle_parse_errors body parse_update
      (fun pol ->
         DynGraph.push pol (get_client clientId).policy_node;
         Cohttp_async.Server.respond `OK)
    | _, _ ->
      printf "Got garbage from Client"; Cohttp_async.Server.respond `Not_found

let listen ~http_port ~openflow_port =
  Async_OpenFlow.OpenFlow0x01.Controller.create ~port:openflow_port ()
  >>= fun controller ->
  let module Controller = NetKAT_Controller.Make (struct
      let controller = controller
    end) in
  let _ = Cohttp_async.Server.create (Tcp.on_port http_port)
    (handle_request Controller.event Controller.send_packet_out Controller.query) in
  let (_, pol_reader) = DynGraph.to_pipe pol in
  let _ = Pipe.iter pol_reader ~f:(fun pol -> Controller.update_policy pol) in
  Controller.start ();
  Deferred.return ()

let () = 
  let str = "{\"pols\": [{\"pols\": [{\"pols\": [{\"pred\": {\"header\": \"location\", \"type\": \"test\", \"value\": {\"type\": \"physical\", \"port\": 2}}, \"type\": \"filter\"}, {\"header\": \"location\", \"type\": \"mod\", \"value\": {\"type\": \"physical\", \"port\": 1}}], \"type\": \"seq\"}, {\"pols\": [{\"pred\": {\"header\": \"location\", \"type\": \"test\", \"value\": {\"type\": \"physical\", \"port\": 3}}, \"type\": \"filter\"}, {\"header\": \"location\", \"type\": \"mod\", \"value\": {\"type\": \"physical\", \"port\": 1}}], \"type\": \"seq\"}], \"type\": \"union\"}, {\"pols\": [{\"pols\": [{\"pred\": {\"preds\": [{\"preds\": [{\"preds\": [{\"preds\": [{\"preds\": [{\"preds\": [{\"header\": \"location\", \"type\": \"test\", \"value\": {\"type\": \"physical\", \"port\": 1}}, {\"header\": \"ethtype\", \"type\": \"test\", \"value\": 2048}], \"type\": \"and\"}, {\"header\": \"ip4src\", \"type\": \"test\", \"value\": {\"mask\": 32, \"addr\": \"10.0.0.1\"}}], \"type\": \"and\"}, {\"header\": \"ip4dst\", \"type\": \"test\", \"value\": {\"mask\": 32, \"addr\": \"10.0.0.2\"}}], \"type\": \"and\"}, {\"header\": \"iproto\", \"type\": \"test\", \"value\": 6}], \"type\": \"and\"}, {\"header\": \"tcpsrcport\", \"type\": \"test\", \"value\": 60437}], \"type\": \"and\"}, {\"header\": \"tcpdstport\", \"type\": \"test\", \"value\": 80}], \"type\": \"and\"}, \"type\": \"filter\"}, {\"header\": \"location\", \"type\": \"mod\", \"value\": {\"type\": \"physical\", \"port\": 2}}], \"type\": \"seq\"}], \"type\": \"union\"}], \"type\": \"union\"}" in 
  ignore (NetKAT_Json.policy_from_json_string str)

let main (http_port : int) (openflow_port : int) () : unit =
  don't_wait_for (listen ~http_port ~openflow_port)
