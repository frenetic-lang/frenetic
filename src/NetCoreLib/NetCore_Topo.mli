type switchId = OpenFlow0x01.switchId
type portId = Packet.portId
type dlAddr = Packet.dlAddr

module type Arg = sig

  val dl_typ : Packet.dlTyp

end

module type TOPO = sig

  val create : (switchId * portId * Packet.bytes -> unit) 
    -> NetCore_Types.pol NetCore_Stream.t * unit Lwt.t

  val ports_of_switch : switchId -> portId list

  val get_switches : unit -> switchId list

end

type loc =
  | Switch of switchId * portId
  | Host of dlAddr

module Make : functor (A : Arg) -> TOPO

(* uses 0x7FF for discovery packets *)
module Topo : TOPO
