type hdr =
  | DlSrc
  | DlDst
  | DlTyp
  | DlVlan
  | DlVlanPcp
  | NwSrc
  | NwDst
  | NwProto
  | NwTos
  | TpSrc
  | TpDst
  | Port
  | Switch

type hdrVal =
  | DlAddr of Packet.dlAddr
  | DlTypVal of Packet.dlTyp
  | DlVlanVal of Packet.dlVlan
  | DlVlanPcpVal of Packet.dlVlanPcp
  | NwAddr of Packet.nwAddr
  | NwTosVal of Packet.nwTos
  | TpPort of Packet.tpPort
  | PortVal of SDN_types.portId
  | SwitchVal of SDN_types.switchId

type pol =
  | Drop
  | Id
  | Test of hdr * hdrVal
  | Set of hdr * hdrVal 
  | Neg of pol
  | Par of pol * pol
  | Seq of pol * pol

(** A map keyed by header names. *)
module HdrMap : Map.S
  with type key = hdr

type hdrValMap = hdrVal HdrMap.t

val format_pol : Format.formatter -> pol -> unit

val string_of_pol : pol -> string

