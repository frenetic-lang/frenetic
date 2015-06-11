open Core.Std
open Async.Std
open Frenetic_NetKAT

let guard (pred: pred) (policy: policy) =
  Seq(Filter pred, policy)

module Net = Frenetic_NetKAT_Net.Net
module Log = Frenetic_Log

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
      | PortUp (switch, port) ->
	  Log.info "Port up event received in switch";
          probes := ({switch_id = switch; port_id = port} :: !probes);
          add_port nib (vertex_of_label nib (Switch switch)) port
      | PortDown (switch, port) ->
          remove_port nib (vertex_of_label nib (Switch switch)) port
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
	 PortMap.fold map ~init:nib ~f:(fun ~key:pt_id ~data:host acc -> 	 	 let h = try Some (vertex_of_label nib (Host (fst host, snd host))) 
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
      Log.info "Port up event! - maybe host down.";
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

end
