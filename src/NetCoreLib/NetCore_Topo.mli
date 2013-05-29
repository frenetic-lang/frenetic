type switchId = OpenFlow0x01.switchId
type portId = Packet.portId
type dlAddr = Packet.dlAddr

(** A network topology that can be inspected using the functions below. *)
type topo

type loc =
  | Switch of switchId * portId
  | Host of dlAddr

(*
type change =
  | SwitchUp of switchId
  | SwitchDown of switchId
  | HostUp of dlAddr
*)
val switches : topo -> switchId list

val hosts : topo -> dlAddr list

(** If [switchId] is not a switch (or a disconnected switch), then
    ports_of_switch returns the empty list. *)
val ports_of_switch : topo -> switchId -> portId list

(** [linked_to topo loc] returns the location that [loc] connects to,
    or [None]. We assume that locations are only linked to a single
    location. *)
val linked_to : topo -> loc -> loc option

(* val changes : topo -> unit Lwt_stream.t *)

(** [make dlTyp] creates a topology-discovery program that uses [dlTyp]
    as the Ethernet type code for its topology-discovery packets.

    Do not use a well-known Ethernet type code. Instead, pick
    something experimental. According to this webpage, codes 0x0101 --
    0x01FF are reserved as experimental:

    http://www.cavebear.com/archive/cavebear/Ethernet/type.html *)

val make : Packet.dlTyp 
  -> NetCore_Types.pol NetCore_Stream.t * 
     (switchId * portId * Packet.bytes) Lwt_stream.t *
     topo
  
