open NetCore_Types
open NetCore_Action.Output

module Log = Frenetic_Log
module Bijection = NetCore_Bijection

type switchId = OpenFlow0x01.switchId
type portId = OpenFlow0x01.portId
type dlAddr = Packet.dlAddr

type loc =
  | Switch of switchId * portId
  | Host of dlAddr

module type Arg = sig

  val dl_typ : Packet.dlTyp

end


module type TOPO = sig

  val create : (switchId * portId * Packet.bytes -> unit Lwt.t) 
    -> NetCore_Types.pol NetCore_Stream.t * unit Lwt.t

  val ports_of_switch : switchId -> portId list

  val get_switches : unit -> switchId list

end


module Make (A : Arg) = struct

  let dl_typ = A.dl_typ

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

  let switch_disconnected edges switches sw =
    try
      let pts = Hashtbl.find switches sw in
      List.iter (fun pt -> Bijection.del edges (Switch (sw, pt))) pts;
      Hashtbl.remove switches sw
    with Not_found -> ()

  let switch_connected edges switches sw pts =
    switch_disconnected edges switches sw;
    Hashtbl.add switches sw pts

  let edges : loc Bijection.t = Bijection.create 100
  
  let switches : (switchId, portId list) Hashtbl.t = Hashtbl.create 10

  let ports_of_switch sw = 
    try Hashtbl.find switches sw with Not_found -> []

  let get_switches () = 
    Hashtbl.fold (fun k _ lst -> k :: lst) switches []

  let switch_event_handler = function
    | SwitchUp (sw, features) -> 
      let ports =
        List.map (fun pt -> pt.OpenFlow0x01.PortDescription.port_no)
          features.OpenFlow0x01.SwitchFeatures.ports in
      switch_connected edges switches sw ports
    | SwitchDown sw -> switch_disconnected edges switches sw

  let recv_discovery_pkt sw pt pk = match pt with
    | Physical phys_pt ->
      begin
        match parse_discovery_pkt pk with
          | None -> Log.printf "NetCore_Topo" "malformed discovery packet.\n%!"
          | Some (sw', pt') -> 
            Log.printf "NetCore_Topo" "added edge %Ld:%d <--> %Ld:%d\n%!"
              sw phys_pt sw' pt';
            Bijection.add edges (Switch (sw, phys_pt)) (Switch (sw', pt'))
      end;
      drop
    | _ -> drop

  let create (send_pkt : lp -> unit Lwt.t) =
    let pol = 
      Union (HandleSwitchEvent switch_event_handler,
               Seq (Filter (Hdr (dlTyp dl_typ)),
                      Action (controller recv_discovery_pkt))) in
    let f sw ports lwt_acc =
      lwt_acc >>
      Lwt_list.iter_s 
        (fun pt -> 
          Log.printf "NetCore_Topo" 
            "emitting a packet to switch=%Ld,port=%d\n%!" sw pt;
            send_pkt (make_discovery_pkt sw pt))
        ports in
    let rec send_discovery () =
      Lwt_unix.sleep 5.0 >>
      Hashtbl.fold f switches Lwt.return_unit >>
      send_discovery () in
    (NetCore_Stream.constant pol, send_discovery ())
      
end

module Topo = Make (struct let dl_typ = 0x7FF end)
