(** NetKAT syntax and semantics. *)

(** {2 Headers} 

    We use the same packet headers that OpenFlow 1.0 supports. Refer to the
    OpenFlow specification for details on their semantics.

    The size of a header depends on the header name. But, insteam encoding this
    dependency in the type system, we place integers of various sizes in the
    [header_val] sum type.

    {b Warning:} Some header combinations are invalid and some headers
    depend on the presence of other headers. To be safe, use {i valid patterns}.

*)


type header = SDN_Headers.header

type header_val = VInt.t

type payload = SDN_Types.payload

(** {2 Policies}

*)

type pred = 
  | True
  | False
  | Test of header*header_val
  | And of pred*pred
  | Or of pred*pred
  | Neg of pred

type policy =
  | Filter of pred
  | Mod of header*header_val
  | Par of policy*policy
  | Choice of policy*policy
  | Seq of policy*policy
  | Star of policy

val id : policy
val drop : policy

(** {2 Packets} 

    If we only defined the semantics and were not building a system, a
    packet would only be a record of headers. However, the runtime needs to
    apply [eval] to packets contained in [PACKET_IN] mesages. For the runtime,
    packets also carry a payload that is unmodified by [eval].

*)

(** A map keyed by header names. *)
module HeaderMap : Map.S
  with type key = header

type header_val_map = header_val HeaderMap.t

type packet = {
  headers : header_val_map;
  (** Evaluating the policy [Test (h, v)] looks for [h] in these headers. If
      they are not found, we signal an error. An OpenFlow switch will never
      look for a header that does not exist. So, it is safe to assume that
       unused headers are set to zero or some other default value. *)
  payload : SDN_Types.payload
}

(** {2 Semantics}

    [eval pkt pol] raises [Not_found] if it tests or updates a header that  [pkt]
    does not have. This behavior is different from OpenFlow, which fails  silently
    in both cases.
*)

module PacketSet : Set.S
  with type elt = packet

module PacketSetSet : Set.S with type elt = PacketSet.t

val eval : packet -> policy -> PacketSetSet.t

(** {2 Utilities} *)

val string_of_header : header -> string

val string_of_vint : VInt.t -> string

val format_policy : Format.formatter -> policy -> unit

val string_of_policy : policy -> string
