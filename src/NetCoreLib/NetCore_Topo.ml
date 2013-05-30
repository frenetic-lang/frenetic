open NetCore_Types
open NetCore_Pattern
open NetCore_Action.Output

module Log = Frenetic_Log

type switchId = OpenFlow0x01.switchId
type portId = Packet.portId
type dlAddr = Packet.dlAddr

type loc =
  | Switch of switchId * portId
  | Host of dlAddr
  | Free

type topo = {
  switches : (switchId, (portId, loc) Hashtbl.t) Hashtbl.t;
  hosts : (dlAddr, loc) Hashtbl.t
}

(*
type change =
  | SwitchUp of switchId
  | SwitchDown of switchId
  | HostUp of dlAddr
*)

let switches topo = 
  Hashtbl.fold (fun sw _ lst -> sw :: lst) topo.switches []

let hosts topo = 
  Hashtbl.fold (fun mac _ lst -> mac :: lst) topo.hosts []

let ports_of_switch topo sw = 
  try
    Hashtbl.fold (fun pt _ lst -> pt :: lst) (Hashtbl.find topo.switches sw) []
  with Not_found ->
    []

let linked_to topo = function
  | Host src -> Hashtbl.find topo.hosts src
  | Switch (sw, pt) -> Hashtbl.find (Hashtbl.find topo.switches sw) pt
  | Free -> Free

let switch_disconnected topo sw = ()

let switch_connected topo sw ports = ()

let new_edge topo (sw1, pt1) (sw2, pt2) = 
  Log.printf "NetCore_Topo" "Detected (%Ld,%d) <--> (%Ld, %d)\n%!" 
    sw1 pt1 sw2 pt2

let make_discovery_pkt dlTyp (sw : switchId) (pt : portId) =
  let open Packet in
  let pk = { 
    dlSrc = 0xffffffffffffL; dlDst = 0xffffffffffffL; dlTyp = dlTyp;
    dlVlan = None; dlVlanPcp = 0;
    nw = Unparsable (Cstruct.of_string (Marshal.to_string (sw, pt) []))
  } in
  (sw, pt, Packet.serialize pk)

let parse_discovery_pkt dlTyp pkt : (switchId * portId) option =
  let open Packet in
  match pkt with
    | { dlSrc = 0xffffffffffffL; dlDst = 0xffffffffffffL; dlTyp = dlTyp;
        dlVlan = None; dlVlanPcp = 0; 
        nw = Unparsable body } ->
      let payload = Cstruct.to_string body in
      Some (Marshal.from_string payload 0)
    | _ -> None

(* Responds to switch up/down events. On switch up, sends a discovery packet,
   and on switch down, cleans up state. *)
let make_switch_handler dlTyp topo = 
  (* switchId * portId * bytes *)
  let (discovery_pkt_stream, send_discovery_pkt) = Lwt_stream.create () in
  let switch_event_handler = function
    | SwitchUp (sw, features) -> 
      let ports =
        List.map (fun pt -> pt.OpenFlow0x01.PortDescription.port_no)
          features.OpenFlow0x01.Features.ports in
      switch_connected topo sw ports;
      Lwt.async
        (fun () ->
          Lwt_unix.sleep 5.0 >>
          (List.iter 
            (fun pt -> 
              Log.printf "NetCore_Topo" 
                "emitting a packet to switch=%Ld,port=%d\n%!" sw pt;
              send_discovery_pkt 
                (Some (make_discovery_pkt dlTyp sw pt)))
            ports;
           Lwt.return ()))
    | SwitchDown sw -> () in
  (discovery_pkt_stream, switch_event_handler)

let recv_discovery_pkt dlTyp topo sw pt pk = 
  match pt with
    | Physical phys_pt ->
      begin
        match parse_discovery_pkt dlTyp pk with
          | None -> Log.printf "NetCore_Topo" "malformed discovery packet.\n%!"
          | Some (sw', pt') -> new_edge topo (sw, phys_pt) (sw', pt')
      end;
      drop
    | _ -> drop


let make pkt_dlTyp =
  let topo = { switches = Hashtbl.create 1;
               hosts = Hashtbl.create 100 
             } in
  let (send_discovery_pkt, handle_switch_up_down) =
    make_switch_handler pkt_dlTyp topo in
  let recv_discovery_pkt = recv_discovery_pkt pkt_dlTyp topo in
  let pol = 
    PoUnion (HandleSwitchEvent handle_switch_up_down,
             PoSeq (PoFilter (PrHdr (dlType pkt_dlTyp)),
                    PoAction (controller recv_discovery_pkt))) in
  (NetCore_Stream.constant pol,
   send_discovery_pkt,
   topo)
