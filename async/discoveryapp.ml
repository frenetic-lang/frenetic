open Core.Std
open Async.Std
open Frenetic_NetKAT
open Cohttp_async
module Log = Frenetic_Log
module Net = Frenetic_NetKAT_Net.Net

let guard (pred: pred) (policy: policy) =
  Seq(Filter pred, policy)

type node = Frenetic_NetKAT_Net.node

type gui_event =
  | AddNode of node 
  | DelNode of node
  | AddLink of node * node 
  | DelLink of node * node 

module Switch = struct
  module Probe = struct
    cstruct probe_payload {
      uint64_t switch_id;
      uint32_t port_id
    } as big_endian

    (* XXX(seliopou): Watch out for this. The protocol in etheret packets has two
     * different meanings depending on the range of values that it falls into. If
     * anything weird happens with probe sizes, look here.  This is not the protocol,
     * but in fact the size.  *)

    let protocol = 0x05ff
    let mac = 0xffeabbadabbaL

    exception Wrong_type

    (* A probe consists of a switch_id and port_id, both represented as int64s
     * regardless of the underlying OpenFlow protocol's representation.
     * [rjjr: info is from the switch to which probe was originally sent] *)
    type t =
      { switch_id : int64
      ; port_id : int32
      }

    let marshal t b =
      set_probe_payload_switch_id b t.switch_id;
      set_probe_payload_port_id b t.port_id;
      sizeof_probe_payload

    let marshal' t =
      let b = Cstruct.create sizeof_probe_payload in
      ignore (marshal t b);
      b

    let parse b =
      { switch_id = get_probe_payload_switch_id b
      ; port_id = get_probe_payload_port_id b
      }

    let of_packet p =
      let open Frenetic_Packet in
      match p.nw with
        | Unparsable(proto, b)
          when proto = protocol -> parse b
        | _ -> raise Wrong_type

    let to_packet t =
      let open Frenetic_Packet in
      { dlSrc = mac
      ; dlDst = 0xffffffffffffL
      ; dlVlan = None
      ; dlVlanDei = false
      ; dlVlanPcp = 0x0
      ; nw = Unparsable(protocol, marshal' t)
      }

    let to_pkt_out t : Frenetic_OpenFlow.pktOut =  
      let bytes = Frenetic_Packet.marshal (to_packet t) in
      let action = Frenetic_OpenFlow.(Output(Physical(t.port_id))) in 
      (NotBuffered(bytes), Some(t.port_id), [action])

  end

  let probes : Probe.t list ref = ref []

  let probe_period = Time.Span.of_sec 3.0

  (*
  let send_probes () =
    let uri = Uri.of_string "http://localhost:8080/pkt_out" in
    Cohttp_async.Client.post 
    *)

  let handle_probe nib dst_swid dst_port (probe : Probe.t) : Net.Topology.t =
    let open Net.Topology in     
    let open Frenetic_NetKAT_Net in 
    let topo, v1 = add_vertex nib (Switch dst_swid) in
    let topo, v2 = add_vertex topo (Switch probe.switch_id) in
    let topo, _ = add_edge topo v1 dst_port Link.default v2 probe.port_id in
    let topo, _ = add_edge topo v2 probe.port_id Link.default v1 dst_port in
    topo

  let update (nib: Net.Topology.t) (evt: event) : Net.Topology.t =
    let open Net.Topology in
    match evt with
      | PacketIn ("probe", switch, port, payload, len) ->
          let open Frenetic_Packet in
          begin match parse (Frenetic_OpenFlow.payload_bytes payload) with
          | { nw = Unparsable (dlTyp, bytes) } when dlTyp = Probe.protocol ->
              let probe = Probe.parse bytes in
              handle_probe nib switch port probe
          | _ -> nib (* error: bad packet *)
          end
      | SwitchUp (switch_id, ports) ->
          List.iter ports (fun port_id ->
            probes := ({switch_id; port_id}) :: !probes);
          let nib', node = add_vertex nib (Switch switch_id) in
          List.fold ports ~init:nib' ~f:(fun nib'' port -> add_port nib'' node port)
      | SwitchDown switch ->
          remove_vertex nib (vertex_of_label nib (Switch switch))
      | PortUp (switch, port) ->(
	  Log.info "%s" "port up received in switch.";
          probes := ({switch_id = switch; port_id = port} :: !probes);
          try (
            let v1 = vertex_of_label nib (Switch switch) in 
            let mh = next_hop nib v1 port in 
            (match mh with
             | None -> nib
             | Some (edg) -> remove_edge nib edg)) 
     	  with _ -> nib)
      | _ -> nib

  let rec probeloop (sender : switchId -> Frenetic_OpenFlow.pktOut -> unit Deferred.t) =
    Clock.after probe_period >>=
      fun () ->
        (Deferred.List.iter ~how:`Parallel (!probes)
          ~f:(fun p ->
            sender p.switch_id (Probe.to_pkt_out p))) >>=
        fun () -> probeloop sender

  let create () : policy =
    let open Frenetic_NetKAT_Optimize in 
    guard (Test(EthSrc Probe.mac)) (Mod(Location(Pipe "probe")))

end

module SwitchMap = Map.Make(Int64)
module PortMap = Map.Make(Int32)
module Host = struct

  let state = ref (SwitchMap.empty) 
 
  let create () : policy = 
    let open Frenetic_NetKAT in
    let open Frenetic_NetKAT_Optimize in
    let default = Test(EthType 0x0806) in 
    let func ~key:sw_id ~data:port_map acc = PortMap.fold port_map ~init:acc ~f:(fun ~key:port ~data:(mac,ip) acc' -> mk_and acc' (Neg(Test(EthSrc mac)))) in 
    let tests = SwitchMap.fold !state ~init:default ~f:func in
    Seq(Filter tests, (Mod(Location(Pipe "host")))) 

  let update (nib: Net.Topology.t) (evt:event) : Net.Topology.t  = 
    let open Net.Topology in
    let open Frenetic_NetKAT in
    match evt with
     | SwitchUp(switch_id,port_id) ->
       state:= SwitchMap.add !state switch_id PortMap.empty;
       nib
     | SwitchDown(switch_id) ->
       (let portmap = SwitchMap.find !state switch_id in 
       let nib = match portmap with 
  | None ->  nib
  | Some map -> 
   PortMap.fold map ~init:nib ~f:(fun ~key:pt_id ~data:host acc ->     let h = try Some (vertex_of_label nib (Host (fst host, snd host))) 
     with _ -> None in 
   match h with 
   | None -> nib
   | Some v -> remove_vertex nib v) in 
  state:= SwitchMap.remove !state switch_id; 
  nib)  
     | PacketIn( "host" ,sw_id,pt_id,payload,len) -> (
  let open Frenetic_Packet in
  Log.info "pinged!";
  let dlAddr,nwAddr = match parse(Frenetic_OpenFlow.payload_bytes payload) with
     | {nw = Arp (Arp.Query(dlSrc,nwSrc,_)) }
     | {nw = Arp (Arp.Reply(dlSrc,nwSrc,_,_)) } ->
         (dlSrc,nwSrc) 
     | _ -> assert false in
    let h = try Some (vertex_of_label nib (Host (dlAddr,nwAddr))) 
        with _ -> None in
  let s = try Some (vertex_of_label nib (Switch sw_id)) 
    with _ -> None in 
    begin match Frenetic_Topology.in_edge nib sw_id pt_id, h , s with
      | true, None, Some sw ->
    Log.info "Edges before adding this: %d" (num_edges nib);
        let nib', h = add_vertex nib (Host (dlAddr,nwAddr)) in
    let module Link = Frenetic_NetKAT_Net.Link in 
        let nib', _ = add_edge nib' sw pt_id Link.default h Int32.one in
        let nib', _ = add_edge nib' h Int32.one Link.default sw pt_id in
    let portmap = SwitchMap.find !state sw_id in 
    let portmap = (match portmap with 
     | Some (map) -> PortMap.add map pt_id (dlAddr,nwAddr)
     | None -> PortMap.empty) in
                 state := SwitchMap.add !state sw_id portmap;
    Log.info "Edges after adding this: %d" (num_edges nib');
     nib'
        | _ , _ , _ -> nib
    end)

    | PortUp (sw_id,pt_id) -> (
      let portmap = SwitchMap.find !state sw_id in
      match portmap with
      | None -> nib
      | Some (map) -> 
    begin
      let host = PortMap.find map pt_id in 
      match host with 
      | None -> nib
      | Some h ->
        let portmap = PortMap.remove map pt_id in 
        state := SwitchMap.add !state sw_id portmap;
    let v2 = vertex_of_label nib (Host (fst h, snd h)) in 
    remove_vertex nib v2
    end)
    | _ -> nib

end

module Events = struct 

  let state = ref []

  let clear_log () = 
    state:= []

  let get_state () = 
    let resp = !state in 
    state := [];
    resp

  (*Note: do this before updating other modules in discovery.
Must process events in reverse order later because appending them.*)

  let update (nib: Net.Topology.t) (evt:event) : unit =
    let open Net.Topology in  
    match evt with
    | SwitchUp(switch_id,port_id) ->
        state:= (AddNode (Switch switch_id))::!state

    | SwitchDown(switch_id) ->
        state:= (DelNode (Switch switch_id))::!state

    | PacketIn( "host" ,sw_id,pt_id,payload,len) -> (
  let open Frenetic_Packet in
  let dlAddr,nwAddr = (match parse(Frenetic_OpenFlow.payload_bytes payload) with
  | {nw = Arp (Arp.Query(dlSrc,nwSrc,_)) }
  | {nw = Arp (Arp.Reply(dlSrc,nwSrc,_,_)) } ->
         (dlSrc,nwSrc) 
  | _ -> assert false) in
    let h = try Some (vertex_of_label nib (Host (dlAddr,nwAddr))) 
      with _ -> None in
    match h with 
    | None -> 
      state:= (AddNode (Host (dlAddr, nwAddr)))::!state;
      state:= (AddLink ((Host (dlAddr,nwAddr)),Switch sw_id))::!state
    | Some host -> ())

    | PacketIn ("probe", sw_id,pt_id,payload,len ) -> (
        let open Frenetic_Packet in
        match parse(Frenetic_OpenFlow.payload_bytes payload) with
        | { nw = Unparsable (dlTyp, bytes) } when dlTyp = Switch.Probe.protocol ->
            (let probe = Switch.Probe.parse bytes in
            let n1:node = Switch (sw_id) in
            let n2:node = Switch (probe.switch_id) in 
	    let v1 = vertex_of_label nib n1 in
	    let v2 = vertex_of_label nib n2 in
	    let e = try Some (find_edge nib v1 v2) 
		with _ -> None in 
	    match e with 
	    | None -> state := (AddLink (n1,n2))::!state
	    | Some x -> ())
        | _ -> ())

    | PortUp (sw_id,pt_id) -> (
      let n1:node = Switch sw_id in 
      let node2 = (try 
        let v1 = vertex_of_label nib (Switch sw_id) in 
        let mh = next_hop nib v1 pt_id in 
        (match mh with
        | None -> None
        | Some (edge) -> 
          let (v2,pt_id2) = edge_dst edge in 
          let open Frenetic_NetKAT_Net in 
          (match (vertex_to_label nib v2) with
           | Switch (sw_id2) -> Some (Switch sw_id2)
           | Host (dl,nw) -> Some (Host(dl,nw))))
      with _ -> None) in 
      match node2 with
      | None -> ()
      | Some n2 -> 
        state:=(DelLink (n1,n2)) :: !state)   
    | _ -> ()
        
end

module Eventjson = struct 
  module Topo = Frenetic_NetKAT_Net.Net.Topology
  module VertexMap = Map.Make(String)

  let vertex_to_json (v: Frenetic_NetKAT_Net.node): Yojson.Safe.json =
    match v with
    | Switch s_id -> 
        `Assoc [("type", `String "switch"); 
                ("id", `Intlit (Int64.to_string s_id))]
    | Host (dladdr, nwaddr) ->
        `Assoc [("type", `String "host");
                ("mac", `String (Frenetic_Packet.string_of_mac dladdr));
                ("ip", `String (Frenetic_Packet.string_of_ip nwaddr))] 

  let vertices_to_json t: Yojson.Safe.json = 
    `List (Topo.VertexSet.fold (Topo.vertexes t) ~f: (fun acc v -> (vertex_to_json (Topo.vertex_to_label t v))::acc) ~init: []) 

  let vertex_idmap t = 
    let vertices = vertices_to_json t in
    let  vlist = Yojson.Basic.Util.to_list (Yojson.Safe.to_basic vertices) in
    let idmap = ref VertexMap.empty in
    let _ = List.iteri vlist ~f: (fun index el-> idmap := VertexMap.add !idmap (Yojson.Basic.to_string el) index;) in
    !idmap 

  let edge_to_json t idmap (e: Topo.edge): Yojson.Safe.json = 
      let src, src_port = Topo.edge_src e in
      let dst, dst_port = Topo.edge_dst e in 
      let src = Topo.vertex_to_label t src in 
      let dst = Topo.vertex_to_label t dst in
      let src_id = VertexMap.find_exn idmap (Yojson.Basic.to_string (Yojson.Safe.to_basic (vertex_to_json src)))  in 
      let dst_id = VertexMap.find_exn idmap (Yojson.Basic.to_string (Yojson.Safe.to_basic (vertex_to_json dst)))  in 
      `Assoc [("src_id", `Int src_id);
              ("src_port", `Int (Int32.to_int_exn src_port));
              ("label", `String "");
              ("dst_id", `Int dst_id);
              ("dst_port", `Int (Int32.to_int_exn dst_port))] 

  let gui_event_to_json idmap t (evt:gui_event) : Yojson.Safe.json= 
  	match evt with 
  	| AddNode (node) ->
  		let v1 = vertex_to_json node in 
  		`Assoc [("type", `String "AddNode");
  				("node", v1)]
  	| DelNode (node) ->
  		let v1 = vertex_to_json node in 
  		`Assoc [("type", `String "DelNode");
  				("node", v1)]
  	| AddLink (n1,n2) ->
      let e = Topo.find_edge t (Topo.vertex_of_label t n1) (Topo.vertex_of_label t n2) in 
  		let edg = edge_to_json t idmap e in 
  		`Assoc [("type", `String "AddLink");
  		("link", edg)]
  	| DelLink (n1,n2) ->
      let e = Topo.find_edge t (Topo.vertex_of_label t n1) (Topo.vertex_of_label t n2) in 
  		let edg = edge_to_json t idmap e in 
  		`Assoc [("type", `String "DelLink");
  		("link", edg)]

  let delta_events_to_json (events:gui_event list) 
  	(t:Frenetic_NetKAT_Net.Net.Topology.t) = 
  	let idmap = vertex_idmap t in
  	`List (List.fold_left events ~init:[] ~f:(fun acc x -> (gui_event_to_json idmap t x)::acc))

  let topo_to_json (t: Frenetic_NetKAT_Net.Net.Topology.t) = 
  	let idmap = vertex_idmap t in 
  	let vertices = vertices_to_json t in
    let edges = `List (Topo.fold_edges (fun e acc -> (edge_to_json t idmap e)::acc) t []) in
    Yojson.Safe.to_string (`Assoc [("nodes", vertices); ("links", edges);])
end

module Discovery = struct 

  type t = {
    nib : Net.Topology.t ref;
    policy : policy;
  }

  let t = { 
    nib = ref (Net.Topology.empty ());
    policy = id;
  }    

  let rec loop (event_pipe: event Pipe.Reader.t) (update_pol:policy -> unit Deferred.t) : unit Deferred.t =
    Pipe.read event_pipe >>= function
      | `Eof -> return ()
      | `Ok evt -> begin
    Events.update !(t.nib) evt;
    t.nib := Switch.update (Host.update !(t.nib) evt) evt;
    let new_pol = Union(Switch.create (), Host.create()) in 
    (*update_pol new_pol >>=
    fun b ->*) loop event_pipe update_pol end 

  let start (event_pipe: event Pipe.Reader.t) (update_pol:policy -> unit Deferred.t)
    (packet_send : switchId -> Frenetic_OpenFlow.pktOut -> unit Deferred.t) =

    let policy = Union (Switch.create (), Host.create ()) in
    let _ = Deferred.both (loop event_pipe update_pol) (Switch.probeloop packet_send) >>|
    fun _ -> ( ) in
    {t with policy}

  let node_data_string pol flowtable =
    let open Yojson.Basic.Util in 
    let flow_json = Yojson.Basic.to_string(Frenetic_NetKAT_SDN_Json.flowTable_to_json flowtable) in 
    Yojson.Basic.to_string (`Assoc[("policy",`String pol);
          ("flowtable",`String flow_json)])

  let show_headers h =
  Cohttp.Header.iter (fun k v -> List.iter v ~f:(Printf.eprintf "%s: %s\n%!" k)) h

let make_req uri meth' () =
  let meth = Cohttp.Code.method_of_string meth' in
  let uri = Uri.of_string uri in
  let headers = Cohttp.Header.of_list [ "connection", "close" ] in 
  Client.call meth ~headers uri
  >>= fun (res, body) ->
  body
  |> Body.to_pipe
  |> Pipe.to_list >>| String.concat

  let start_server (http_port : int) update_policy : unit =  

    let module StatMap = Map.Make(Int) in 
    let track = ref false in 
    let track_name = ref "" in 
    let stats = ref StatMap.empty in 
    
    let rec collect_stats name = 
       (if (!track) then (
    Clock.after (Time.Span.of_sec 4.0) >>= fun () ->(
      let cur_time = Float.to_int (Unix.gettimeofday ()) in 
      make_req ("http://localhost:9000/query/"^name) "GET" ()  >>= fun data ->(
	let statstr = Yojson.Basic.from_string data in
        stats := StatMap.add !stats cur_time statstr; 
        collect_stats name))
    )
       else (return ())) in 

    let routes = [
      ("/topology", fun _ -> 
	Events.clear_log ();
	return (Gui_Server.string_handler (Eventjson.topo_to_json !(t.nib))));
      ("/switch/([1-9][0-9]*)", fun g -> 
        let sw_id = Array.get g 1 in
        make_req "http://localhost:9000/policy" "GET" () >>=
        fun pol ->
	  let pol = Frenetic_NetKAT_Json.policy_from_json_string pol |>
	  Frenetic_NetKAT_Pretty.string_of_policy in 
          make_req ("http://localhost:9000/flowtbl/"^sw_id) "GET" () >>=
          fun flowtbl ->  let data = Yojson.Basic.to_string (`Assoc[("policy",`String pol);
          ("flowtable",`String flowtbl)]) in 
          return (Gui_Server.string_handler data));
      ("/switch/([1-9][0-9]*)/port/([1-9][0-9]*)", fun g ->
        let sw_id = Array.get g 1 in
        let pt_id = Array.get g 2 in
        let uri = "http://localhost:9000/port_stats/" ^ sw_id ^ "/" ^ pt_id in
        make_req uri "GET" () >>= fun stats ->
          return (Gui_Server.string_handler stats));
      ("/query/(.*)/pred/(.*)", fun g -> 
        let name = Array.get g 1 in 
        make_req ("http://localhost:9000/is_query/"^name) "GET" () >>= 
        fun b -> if (b = "true") then 
          return (Gui_Server.string_handler "Already Exists.")
        else(
          let polstr = Array.get g 2 in 
          let replace re t s= Str.global_replace (Str.regexp_string re) t s in
          let polstr = replace "%20" " " polstr |>
                      replace "%3A" ":" |>
                      replace "%7B" ";" in
	  Log.info "%s" polstr;
          let policy = try Some (Frenetic_NetKAT_Parser.policy_from_string polstr)
                with _ -> None in 
          match policy with 
          | Some pol -> begin
          let query = Seq (pol, (Mod(Location(Query name)))) in 
          make_req "http://localhost:9000/policy" "GET" () >>= 
          fun old_polstr -> 
	    let old_pol = Frenetic_NetKAT_Json.policy_from_json_string old_polstr in
            Log.info "%s" "No trouble parsing!";
	    Union (query, old_pol) |> 
            update_policy  >>= fun _ -> 
              return (Gui_Server.string_handler "Query added.") 
            end
          | None -> return (Gui_Server.string_handler "Invalid policy.")
        ));
      ("/stats/(.*)", fun g ->
        let name = Array.get g 1 in 
        make_req ("http://localhost:9000/is_query/"^name) "GET" () >>= fun r ->
          if (r = "true") then begin
            make_req ("http://localhost:9000/query/"^name) "GET" () >>= fun stats ->
              return (Gui_Server.string_handler stats)
            end
          else 
            return (Gui_Server.string_handler "No such query.")
      );
      ("/track/(.*)", fun g ->
        let name = Array.get g 1 in 
        make_req ("http://localhost:9000/is_query/"^name) "GET" () >>= fun re ->
          if(re = "true") then 
            if(!track = false ) then (
              track_name := name;
              track := true;
              stats := StatMap.empty;
              don't_wait_for (collect_stats !track_name);
              return (Gui_Server.string_handler "collecting stats.")
            )else(
              track := false;
              return (Gui_Server.string_handler ("Stopped tracking"^ !track_name))
            ) 
          else 
            return (Gui_Server.string_handler "No such query!"));
      ("/graph", fun _ ->
         let json_stat time dp = `Assoc [("time", `Int time);("stat", dp)] in
         let data = `List (StatMap.fold !stats ~init:[] ~f:(fun ~key:time ~data:stat acc-> (json_stat time stat) :: acc)) in
         return (Gui_Server.string_handler (Yojson.Basic.to_string data )));
      ("/events", fun _ -> 
	 let events = Events.get_state () in 
	 let data = Eventjson.delta_events_to_json events !(t.nib) in
	 Log.info "events: %s" (Yojson.Safe.to_string data);
	 return (Gui_Server.string_handler (Yojson.Safe.to_string data)));
    ] in
    let _ = Gui_Server.create routes in
    ()

end 
