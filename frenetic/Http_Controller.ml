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

(* Simply blocks indefinitely on a pipe-reader. Hides the EOF exception. *)
let long_poll (reader : 'a Pipe.Reader.t) : 'a Deferred.t =
  Pipe.read reader >>= (function
  | `Ok v -> Deferred.return v
  | `Eof -> failwith "Eof reading pipe in long_poll")

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
      handle_parse_errors body
        (fun body ->
           Body.to_string body >>= fun str ->
           let json = Yojson.Basic.from_string str in
           return (NetKAT_SDN_Json.pkt_out_from_json json))
        (fun pkt_out ->
           printf "POST /pkt_out";
           Pipe.write send.pkt_out pkt_out >>= fun _ ->
           Cohttp_async.Server.respond `OK)
    | `POST, [clientId; "update" ] ->
      handle_parse_errors body parse_update
      (fun pol ->
         printf "POST /%s/update\n%s\n%!" clientId (NetKAT_Pretty.string_of_policy pol);
         DynGraph.push pol (get_client clientId).policy_node;
         Cohttp_async.Server.respond `OK)
    | _, _ -> printf "Got garbage from Client"; Cohttp_async.Server.respond `Not_found

let listen ~port =
  let pipes = Async_NetKAT.PipeSet.singleton "http" in
  let http_app = Async_NetKAT.Policy.create_async ~pipes:pipes NetKAT_Types.drop
    (fun topo send () ->
       let (_, pol_reader) = DynGraph.to_pipe pol in
       let _ = Pipe.transfer_id pol_reader send.update in
       let _ = Cohttp_async.Server.create (Tcp.on_port port)
                 (handle_request send) in
       fun event ->
         (* TODO(arjun): save event so it can be long-polled later (wtf) *)
         printf "Adding event to pipe: %s\n%!" (NetKAT_Json.event_to_json_string event);
         iter_clients (fun _ client ->
           Pipe.write_without_pushback client.event_writer event);
         return None) in
  don't_wait_for
    (Async_NetKAT_Controller.start http_app () >>= fun ctrl ->
     Async_NetKAT_Controller.disable_discovery ctrl)

let main (args : string list) : unit = match args with
  | [ "--app-port"; p ] | [ "-a"; p ] ->
    listen ~port:(Int.of_string p)
  | [] -> listen ~port:9000
  |  _ -> (print_endline "Invalid command-line arguments"; Shutdown.shutdown 1)
