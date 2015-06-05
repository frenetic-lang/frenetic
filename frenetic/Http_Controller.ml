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

let pol : (policy, policy) DynGraph.t = DynGraph.create drop unions

let clients : (string, client) Hashtbl.t = Hashtbl.Poly.create ()

let iter_clients (f : string -> client -> unit) : unit =
  Hashtbl.iter clients ~f:(fun ~key ~data -> f key data)

type upd_flag =
  | Blocked of string list
  | Restart of (string * policy option) list
  | Normal

let paused = ref Normal (* TODO where to put this? *)

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

let normal_resp body pol clientId = 
  handle_parse_errors body parse_update_json
  (fun pol ->
     DynGraph.push pol (get_client clientId).policy_node;
     Cohttp_async.Server.respond `OK)


let handle_request
  (module Controller : NetKAT_Controller.CONTROLLER)
  ~(body : Cohttp_async.Body.t)
  (client_addr : Socket.Address.Inet.t)
  (request : Request.t) : Server.response Deferred.t =
  let open Controller in
  Log.info "%s %s" (Cohttp.Code.string_of_method request.meth)
    (Uri.path request.uri);
  match request.meth, extract_path request with
    | `GET, ["version"] -> Server.respond_with_string "3"
    | `GET, ["port_stats"; switch_id; port_id] ->
       port_stats (Int64.of_string switch_id) (Int32.of_string port_id)
       >>= fun portStats ->
       Server.respond_with_string (NetKAT_Json.port_stats_to_json_string portStats)
    | `GET, ["current_switches"] ->
      let switches = current_switches () in
      Server.respond_with_string (current_switches_to_json_string switches)
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
      (match !paused with
        | Blocked x -> 
          (match List.exists x (fun curr_id-> curr_id=clientId) with
            | true -> 
                printf ~level:`Error "Currently Paused For Update";
                Cohttp_async.Server.respond `Bad_request
            | false -> normal_resp body pol clientId) 
        | Restart pol_list -> 
          (match List.exists pol_list (fun (curr_id, _) -> curr_id=clientId) with
            | true -> 
                begin
                  match List.exists pol_list 
                    (fun (curr_id, p) -> ((curr_id=clientId) && (p=None))) with
                    | true ->
                        let tmp = ref None in (*TODO This is an abomination. why can't I fix IO.t???*)
                        Body.to_string body >>= fun str -> 
                        tmp := Some (NetKAT_Json.policy_from_json_string str); (* TODO very bad.*)
                        paused := Restart ((clientId, !tmp):: (*This stores the policy!!!*)
                        (List.filter pol_list (fun (c,_) -> c<>clientId)));
                        printf ~level:`Error "(Not error, just wanted red font) Adding policy for __%s__, held for after resume%!"  clientId;
                          Cohttp_async.Server.respond `OK
                    | false -> match (List.exists pol_list (fun (_, p) -> p=None)) with
                        | true ->   (* Waiting for other apps to push their rules*)
                          printf ~level:`Error "(Not error, just wanted red font) Waiting for other apps to push rules, blocking %s%!" clientId;
                          Cohttp_async.Server.respond `OK
                        | false -> 
                            begin (* have all necessary stored rules. now send them all together*)
                              printf ~level:`Error "(not error, just red) HERE COMES SOME RULES!!!!%!";
                              List.iter pol_list (fun (id, pol) -> match pol with
                                | Some p -> DynGraph.push p (get_client id).policy_node
                                | _ ->  printf ~level:`Error "Something horribly wrong...");
                              paused := Normal;  (* Now resume normal execution *)
                              print_endline ">>>>>>>>>>>>MUX: Sent Rules all in one batch.";
                              Cohttp_async.Server.respond `OK
                            end
                end
            | false -> normal_resp body pol clientId) 
        | Normal -> normal_resp body pol clientId )
    | `POST, [clientId; "update" ] ->
      handle_parse_errors body parse_update
      (fun pol ->
         DynGraph.push pol (get_client clientId).policy_node;
         Cohttp_async.Server.respond `OK)
    | `POST, ["upd_chan_pause" ] ->
      print_endline ">>>>>>>>>>>>MUX: Blocking.";
      Body.to_string body >>= fun str ->
      let open Yojson.Basic.Util in
      let j = (Yojson.Basic.from_string str) in 
      paused := Blocked (filter_string (to_list j)) ;
      Cohttp_async.Server.respond `OK
    | `POST, ["upd_chan_resume" ] -> (match !paused with
       | Blocked l -> paused := Restart (List.map l (fun id -> (id, None)))
       | _ -> printf ~level:`Error "No list to unpause.");
      print_endline ">>>>>>>>>>>>MUX: Ready to queue up rules, still blocking.";
      Cohttp_async.Server.respond `OK
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

