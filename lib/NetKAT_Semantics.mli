open Core.Std

open NetKAT_Types
open Packet

(** {2 Packets}

  If we only defined the semantics and were not building a system, a
  packet would only be a record of headers. However, the runtime needs to
  apply [eval] to packets contained in [PACKET_IN] mesages. For the runtime,
  packets also carry a payload that is unmodified by [eval]. *)

(** A map keyed by header names. *)
module HeadersValues : sig
  type t =
    { location : location
    ; ethSrc : dlAddr
    ; ethDst : dlAddr
    ; vlan : int16
    ; vlanPcp : dlVlanPcp
    ; ethType : dlTyp
    ; ipProto : nwProto
    ; ipSrc : nwAddr
    ; ipDst : nwAddr
    ; tcpSrcPort : tpPort
    ; tcpDstPort : tpPort
    } with fields

  val compare : t -> t -> int
  val to_string : t -> string
end

type packet = {
  switch : switchId;
  headers : HeadersValues.t;
  payload : payload
}

module PacketSet : Set.S
  with type Elt.t = packet
    
(** {2 Semantics}
 
  [eval pkt pol] raises [Not_found] if it tests or updates a header that  [pkt]
  does not have. This behavior is different from OpenFlow, which fails  silently
  in both cases. *)
  
val eval : packet -> policy -> PacketSet.t
val eval_pipes :  packet
               -> policy
               -> (string * packet) list * (string * packet) list * packet list

val size: policy -> int
