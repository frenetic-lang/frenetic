(** NetKAT syntax and semantics. *)


(** {2 Headers} 

  We use the same packet headers that OpenFlow 1.0 supports. Refer to the
  OpenFlow specification for details on their semantics.
  
  The size of a header depends on the header name. But, insteam encoding this
  dependency in the type system, we place integers of various sizes in the
  [hdrVal] sum type.

  {b Warning:} Some header combinations are invalid and some headers
  depend on the presence of other headers. To be safe, use {i valid patterns}.

 *)

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
  | Int64 of Int64.t
  | Int48 of Int64.t
  | Int32 of Int32.t
  | Int16 of int
  | Int4 of int

(** {2 Policies}

  In the spirit of KAT, predicates are formed with [Drop], [Id], [Par], [Seq],
  and [Neg].

 *)

type pol =
  | Drop
  | Id
  | Test of hdr * hdrVal
  | Set of hdr * hdrVal 
  | Neg of pol
  | Par of pol * pol
  | Seq of pol * pol

(** {2 Packets} 

  If we only defined the semantics and were not building a system, a
  packet would only be a record of headers. However, the runtime needs to
  apply [eval] to packets contained in [PACKET_IN] mesages. For the runtime,
  packets also carry a payload that is unmodified by [eval].

*)

(** A map keyed by header names. *)
module HdrMap : Map.S
  with type key = hdr

type hdrValMap = hdrVal HdrMap.t

type pkt = {
  headers : hdrValMap;
  (** Evaluating the policy [Test (h, v)] looks for [h] in these headers. If
      they are not found, we signal an error. An OpenFlow switch will never
      look for a header that does not exist. So, it is safe to assume that
       unused headers are set to zero or some other default value. *)
  payload : SDN_types.payload
}

(** {2 Semantics}

  [eval pkt pol] raises [Not_found] if it tests or updates a header that  [pkt]
  does not have. This behavior is different from OpenFlow, which fails  silently
  in both cases.
*)

module PktSet : Set.S
  with type elt = pkt

val eval : pkt -> pol -> PktSet.t

(** {2 Utilities} *)

val format_pol : Format.formatter -> pol -> unit

val string_of_pol : pol -> string
