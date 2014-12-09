open Core.Std
open Async.Std
open Cohttp_async
open NetKAT_Types
module Server = Cohttp_async.Server

type client = {
  (* Write new policies to this node *)
  policy_node: (DynGraph.cannot_receive, policy) DynGraph.t;
  (* Read from this pipe to send events *)
  event_reader: event Pipe.Reader.t;
  (* Write to this pipe when new event received from the network *)
  event_writer: event Pipe.Writer.t
}

(* Output log messages tagged (http, http) *)
let _ =
  let open Async_OpenFlow.Log in
  set_output [make_filtered_output [("http", "http")]];
  set_level `Info

let printf ?(level: [ `Debug | `Info | `Error ] = `Info) (fmt :  ('a, unit, string, unit) format4) =
  Async_OpenFlow.Log.printf ~level ~tags:[("http", "http")] fmt


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


(* Extract the path, split on slashes, and remove empty strings caused by
   repeated slashes *)
let extract_path (req : Request.t) : string list =
    List.filter ~f:(fun str -> not (String.is_empty str))
      (String.split ~on:'/'
         (Uri.path req.uri))

(* Simply blocks indefinitely on a pipe-reader. Hides the EOF exception. *)
let long_poll (reader : 'a Pipe.Reader.t) : 'a Deferred.t =
  Pipe.read reader >>= (function
  | `Ok v -> Deferred.return v
  | `Eof -> failwith "Eof reading pipe in long_poll")


(* Note: cannot create a variable called parser because of some Camlp4 thing
   we are using. *)
let handle_parse_errors
  (body : Cohttp_async.Body.t)
  (body_parser : Cohttp_async.Body.t -> 'a Deferred.t)
  (handler : 'a -> Cohttp_async.Server.response Deferred.t) :
  Cohttp_async.Server.response Deferred.t =
  try_with (fun () -> body_parser body)
  >>= function
  | Ok x -> handler x
  | Error exn ->
      printf "Error parsing message from client";
      Cohttp_async.Server.respond `Bad_request

let handle_request
  (send : policy Async_NetKAT.send)
  ~(body : Cohttp_async.Body.t)
  (client_addr : Socket.Address.Inet.t)
  (request : Request.t) : Server.response Deferred.t =
  match request.meth, extract_path request with
    | `GET, [clientId; "event"] ->
      printf "GET /event";
      long_poll (get_client clientId).event_reader
      >>= fun evt ->
      Server.respond_with_string (NetKAT_Json.event_to_json_string evt)
    | `POST, ["pkt_out"] ->
      handle_parse_errors body NetKAT_Json.parse_pkt_out
      (fun pkt_out ->
         printf "POST /pkt_out";
         Pipe.write send.pkt_out pkt_out >>= fun _ ->
         Cohttp_async.Server.respond `OK)
    | `POST, [clientId; "update" ] ->
      handle_parse_errors body NetKAT_Json.parse_update
      (fun pol ->
         printf "POST /%s/update\n%s\n%!" clientId (NetKAT_Pretty.string_of_policy pol);
         DynGraph.push pol (get_client clientId).policy_node;
         Cohttp_async.Server.respond `OK)
    | _, _ -> printf "Got garbage from Client"; Cohttp_async.Server.respond `Not_found

let listen ?(port=9000) =
  let pipes = Async_NetKAT.PipeSet.singleton "http" in
  Async_NetKAT.Policy.create_async ~pipes:pipes NetKAT_Types.drop
    (fun topo send () ->
       let (_, pol_reader) = DynGraph.to_pipe pol in
       let _ = Pipe.transfer_id pol_reader send.update in
       let (event_reader, event_writer) = Pipe.create () in
       let closed = Cohttp_async.Server.create (Tcp.on_port port)
                      (handle_request send) in
       fun event ->
         (* TODO(arjun): save event so it can be long-polled later (wtf) *)
         printf "Adding event to pipe: %s\n%!" (NetKAT_Json.event_to_json_string event);
         iter_clients (fun _ client ->
           Pipe.write_without_pushback client.event_writer event);
         return None)


let main () : unit =
  printf ~level:`Info "Starting frenetic-http...";
  let http_app = listen ~port:9000 in
  don't_wait_for
    (Async_NetKAT_Controller.start http_app () >>= fun ctrl ->
     Async_NetKAT_Controller.disable_discovery ctrl)

let () =
  never_returns (Scheduler.go_main ~max_num_open_file_descrs:4096 ~main ())
