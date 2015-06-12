open Core.Std
open Async.Std
open Cohttp_async
open Frenetic_NetKAT
open Frenetic_Common
module Server = Cohttp_async.Server
module Log = Frenetic_Log

type client = {
  (* Write new policies to this node *)
  policy_node: (Frenetic_DynGraph.cannot_receive, policy) Frenetic_DynGraph.t;
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
    | `GET, ["version"] -> Server.respond_with_string "3"
    | `GET, ["port_stats"; switch_id; port_id] ->
       port_stats (Int64.of_string switch_id) (Int32.of_string port_id)
       >>= fun portStats ->
       Server.respond_with_string (Frenetic_NetKAT_Json.port_stats_to_json_string portStats)
    | `GET, ["current_switches"] ->
      let switches = current_switches () in
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
    | `GET, ["policy"] -> get_policy () |> 
	Frenetic_NetKAT_Pretty.string_of_policy |>
	Server.respond_with_string
    | _, _ ->
      Log.error "Unknown method/path (404 error)";
      Cohttp_async.Server.respond `Not_found

let print_error addr exn =
  Log.error "%s" (Exn.to_string exn)

type t = (module Frenetic_NetKAT_Controller.CONTROLLER)


let port_stats (t : t) = 
  let module Controller = (val t) in Controller.port_stats 

let current_switches (t : t) =
  let module Controller = (val t) in
  Controller.current_switches () |> return

let query (t : t) name =
  let module Controller = (val t) in
  if (Controller.is_query name) then Some (Controller.query name)
  else None

let event (t : t) clientId =
  let module Controller = (val t) in
  (get_client clientId).event_reader |> Pipe.read >>| function
    | `Eof -> assert false
    | `Ok response -> response

let pkt_out (t:t) = 
  let module Controller = (val t) in 
  Controller.send_packet_out 

let update _ clientId pol = 
  return (Frenetic_DynGraph.push pol (get_client clientId).policy_node)
(*
let listen ~http_port ~openflow_port =
  let module Controller = Frenetic_NetKAT_Controller.Make in
  let on_handler_error = `Call print_error in
  let _ = Cohttp_async.Server.create
    ~on_handler_error
    (Tcp.on_port http_port)
    (handle_request (module Controller)) in
  let (_, pol_reader) = Frenetic_DynGraph.to_pipe pol in
  let _ = Pipe.iter pol_reader ~f:(fun pol -> Controller.update_policy pol) in
  Controller.start ();
  don't_wait_for(propogate_events Controller.event);
  Deferred.return () *)


let start (http_port : int) (openflow_port : int) () : unit =  
  let module Controller = Frenetic_NetKAT_Controller.Make in
  let on_handler_error = `Call print_error in
  let _ = Cohttp_async.Server.create
    ~on_handler_error
    (Tcp.on_port http_port)
    (handle_request (module Controller)) in
  let (_, pol_reader) = Frenetic_DynGraph.to_pipe pol in
  let _ = Pipe.iter pol_reader ~f:(fun pol -> Controller.update_policy pol) in
  Controller.start ~port:openflow_port ();
  Log.info "port is: %d" openflow_port;
  let t:(module Frenetic_NetKAT_Controller.CONTROLLER) = (module Controller) in

  let node_data_string pol flowtable = begin
    let open Yojson.Basic.Util in 
    let flow_json = Yojson.Basic.to_string(Frenetic_NetKAT_SDN_Json.flowTable_to_json flowtable) in 
    Yojson.Basic.to_string (`Assoc[("policy",`String pol);
		      ("flowtable",`String flow_json)])
    end  in

  (* initialize discovery *)
  let discoverclient = get_client "discover" in
  let discover =
    (let event_pipe = Pipe.map discoverclient.event_reader
      ~f:(fun s -> s |> Yojson.Basic.from_string |> Frenetic_NetKAT_Json.event_from_json) in
    Discoveryapp.Discovery.start event_pipe (update t "discover") (pkt_out t)) in
  let _ = update t "discover" discover.policy >>| 
  fun _ ->   
   let module StatMap = Map.Make(String) in 
   let track = ref false in 
   let track_name = ref "" in 
   let stats = ref StatMap.empty in 
   let rec collect_stats name = 
     (if (!track) then (
	Clock.after (Time.Span.of_sec 4.0) >>= fun () ->(
	  let cur_time = Float.to_int (Unix.gettimeofday ()) in 
	  Controller.query name >>= fun data ->(
	    let statstr = Frenetic_NetKAT_Json.stats_to_json_string data in
	    stats := StatMap.add !stats statstr cur_time; 
	    collect_stats name))
	)
     else (return ())) in 
   (let routes = [
    ("/topology", fun _ ->
      return (Gui_Server.string_handler (Gui_Server.topo_to_json !(discover.nib))));
    ("/graph", fun _ ->	
      let json_stat time dp = `Assoc [("time", `Int time);("stat", `String dp)] in
      let data = `List (StatMap.fold !stats ~init:[] ~f:(fun ~key:stat ~data:time acc-> (json_stat time stat) :: acc)) in
      return (Gui_Server.string_handler (Yojson.Basic.to_string data ))
	);
    ("/query/(.*)/pred/(.*)", fun g -> 
	let name = Array.get g 1 in
	if (Controller.is_query name) then 
	  return (Gui_Server.string_handler "Already Exists.")
	else (
	let polstr = Array.get g 2 in 
	let replace re t s= Str.global_replace (Str.regexp_string re) t s in
	let polstr = replace "%20" " " polstr |>
		replace "%3A" ":" |>
		replace "%7B" ";" in
	let pol = Frenetic_NetKAT_Parser.policy_from_string polstr in 
	let query = Seq (pol, (Mod(Location(Query name)))) in 
	let new_pol = Union (query, Controller.get_policy ()) in 
	Controller.update_policy new_pol >>= fun _ -> 
	  return (Gui_Server.string_handler "Ok!"))
	);
    ("stats/(.*)", fun g ->
	let name = Array.get g 1 in 
	if (Controller.is_query name) then begin
	  Controller.query name >>= fun stats ->
	    let str = Frenetic_NetKAT_Json.stats_to_json_string stats in
	    return (Gui_Server.string_handler str) 
	  end
	else 
	    return (Gui_Server.string_handler "No such query.")
	);
    ("/track/(.*)", fun g ->
	let name = Array.get g 1 in 
	if (Controller.is_query name) then (
	  if (!track = false) then (
	   track_name := name;
	   track := true; 
	   don't_wait_for (collect_stats name);
	   return (Gui_Server.string_handler "collecting stats."))
	  else (
	   track := false; 
	   return (Gui_Server.string_handler ("Stopped tracking" ^ !track_name))))
 	else 
	   return (Gui_Server.string_handler "No such query.")
);
    ("/switch/([1-9][0-9]*)", fun g ->
        let sw_id = Int64.of_string (Array.get g 1) in
        printf "Requested policy for switch %Lu" sw_id;
        let pol = Frenetic_NetKAT_Pretty.string_of_policy (Controller.get_policy ()) in
	let flow_table = List.fold_left (Controller.get_table sw_id) ~f:(fun acc x -> (fst x) :: acc) ~init:[] in
        return (Gui_Server.string_handler (node_data_string pol flow_table)));
    ("/switch/([1-9][0-9]*)/port/([1-9][0-9]*)", fun g ->
	Log.info "matched the link route."; 
	let sw_id = Int64.of_string (Array.get g 1) in 
	let pt_id = Int32.of_string (Array.get g 2) in 
	let toint x = Int64.to_int_exn x in
	Controller.port_stats sw_id pt_id >>| fun pstats ->
	  (let rbytes = toint pstats.rx_bytes in 
	  let tbytes =  toint pstats.tx_bytes in
	  let rpackets = toint pstats.rx_packets in 
	  let tpackets = toint pstats.tx_packets in 
	  let data = Yojson.Basic.to_string (`Assoc[("bytes", `String (Int.to_string (rbytes+tbytes))); ("packets",`String (Int.to_string (tpackets + rpackets)))]) in
	Gui_Server.string_handler data));] in
  let _ = Gui_Server.create routes in 
  don't_wait_for (propogate_events Controller.event)) in
  ()

let main (http_port : int) (openflow_port : int) () : unit =
  start http_port openflow_port ()


