open NetCore_Types
open NetCore_Action.Output
open NetCore_Pattern

module G = NetCore_Graph.Graph
module Log = Lwt_log

type switchId = NetCore_Types.switchId
type portId = NetCore_Types.portId
type dlAddr = Packet.dlAddr

module type Arg = sig

  val dl_typ : Packet.dlTyp

end


module type TOPO = sig

  val create : (switchId * portId * Packet.bytes -> unit Lwt.t) 
    -> NetCore_Types.pol NetCore_Stream.t * unit Lwt.t

  val graph : G.graph

  val ports_of_switch : switchId -> portId list

  val edge_ports_of_switch : switchId -> portId list

  val get_switches : unit -> switchId list

end


module Make (A : Arg) = struct

  let dl_typ = A.dl_typ
  let graph = G.create ()

  let parse_discovery_pkt pkt : (switchId * portId) option =
    let open Packet in
    match pkt with
    | { dlSrc = 0xffffffffffffL; 
        dlDst = 0xffffffffffffL; 
        dlVlan = None; dlVlanPcp = 0; 
        nw = Unparsable (dl_typ, body) } ->
      let payload = Cstruct.to_string body in
      Some (Marshal.from_string payload 0)
    | _ -> None

  let make_discovery_pkt  (sw : switchId) (pt : portId) =
    let open Packet in
    let pk = { 
      dlSrc = 0xffffffffffffL;
      dlDst = 0xffffffffffffL;
      dlVlan = None;
      dlVlanPcp = 0;
      nw = Unparsable 
          (dl_typ, Cstruct.of_string (Marshal.to_string (sw, pt) []))
    } in
    (sw, pt, Packet.marshal pk)

  type lp = switchId * portId * Packet.bytes

  let switch_disconnected graph sw =
    try
      G.del_node graph (G.Switch sw)
    with Not_found -> ()

  let switch_connected graph sw pts =
    switch_disconnected graph sw;
    G.add_switch graph sw;
    List.iter (fun pt -> G.add_port graph (G.Switch sw) pt) pts

  (* let edges : loc Bijection.t = Bijection.create 100 *)

  (* let switches : (switchId, portId list) Hashtbl.t = Hashtbl.create 10 *)

  let ports_of_switch sw = 
    G.ports_of_switch graph (G.Switch sw)

  let edge_ports_of_switch sw = ports_of_switch sw

  let get_switches () = G.get_switches graph

  let switch_event_handler = function
    | SwitchUp (sw, features) -> 
      Log.warning_f "switch_up: %s%!\n" (NetCore_Types.string_of_switchId sw);
      Log.warning_f "ports: %s%!\n" (String.concat ";" (List.map NetCore_Types.string_of_portId features.ports));
      switch_connected graph sw features.ports
    | SwitchDown sw -> switch_disconnected graph sw

  let recv_discovery_pkt sw pt pk = match pt with
    | Physical phys_pt ->
      begin
        match parse_discovery_pkt pk with
        | None -> 
          Lwt.async (fun () -> Log.info "malformed discovery packet.\n%!")
        | Some (sw', pt') -> 
          Lwt.async
            (fun () ->
               Log.info_f "added edge %Ld:%ld <--> %Ld:%ld\n%!"
                 sw phys_pt sw' pt');
          G.add_edge graph (G.Switch sw) phys_pt (G.Switch sw') pt'
      end;
      drop
    | _ -> drop

  let create (send_pkt : lp -> unit Lwt.t) =
    let pol = 
      Union (HandleSwitchEvent switch_event_handler,
             Seq (Filter (Hdr (dlTyp dl_typ)),
                  Action (controller recv_discovery_pkt))) in
    let f lwt_acc (sw, ports) =
      lwt_acc >>
      Lwt_list.iter_s 
        (fun pt -> 
           Log.warning_f
             "emitting a packet to switch=%Ld,port=%ld\n%!" sw pt >>
           send_pkt (make_discovery_pkt sw pt))
        ports in
    let rec send_discovery () =
      Lwt_unix.sleep 5.0 >>
      List.fold_left f Lwt.return_unit (G.get_switches_and_ports graph) >>
      send_discovery () in
    (NetCore_Stream.constant pol, send_discovery ())

end

module Topo = Make (struct let dl_typ = 0x7FF end)
