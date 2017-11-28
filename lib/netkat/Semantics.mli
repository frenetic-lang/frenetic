open Core

open Syntax
open Frenetic_kernel.Packet

(** {2 Frenetic_kernel.Packets}

  If we only defined the semantics and were not building a system, a
  packet would only be a record of headers. However, the runtime needs to
  apply [eval] to packets contained in [PACKET_IN] mesages. For the runtime,
  packets also carry a payload that is unmodified by [eval]. *)

(** A map keyed by header names. *)
module HeadersValues : sig
  type t =
    { location : location
    ; from : abstract_location
    ; abstractLoc : abstract_location
    ; ethSrc : dlAddr
    ; ethDst : dlAddr
    ; vlan : int16
    ; vlanPcp : dlVlanPcp
    ; vswitch : int64
    ; vport : int64
    ; ethType : dlTyp
    ; ipProto : nwProto
    ; ipSrc : nwAddr
    ; ipDst : nwAddr
    ; tcpSrcPort : tpPort
    ; tcpDstPort : tpPort
    } [@@deriving sexp, fields]

  val compare : t -> t -> int
  val to_string : t -> string
  val to_hvs : t -> header_val list
end

type packet = {
  switch : switchId;
  headers : HeadersValues.t;
  payload : payload
}

module PacketSet : Set.S
  with type Elt.t = packet

(** {2 Semantics} *)

val eval : packet -> policy -> PacketSet.t
val eval_pipes :  packet
               -> policy
               -> (string * packet) list * (string * packet) list * packet list

val size: policy -> int

val queries_of_policy: policy -> string list

val switches_of_policy: policy -> switchId list
