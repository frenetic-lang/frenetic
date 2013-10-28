(** NetKAT syntax and semantics. *)

module type HEADERS = sig
  type header
  type value
  type payload

  val format_header : Format.formatter -> header -> unit
  val format_value : Format.formatter -> value -> unit
  val header_to_string : header -> string
  val value_to_string : value -> string
  val compare_header : header -> header -> int
end

module type S = sig

  type header
  type header_val
  type payload

  (** {2 Policies} *)

  type pred = 
    | True
    | False
    | Test of header * header_val
    | And of pred * pred
    | Or of pred * pred
    | Neg of pred

  type policy =
    | Filter of pred
    | Mod of header * header_val
    | Par of policy * policy
    | Choice of policy * policy
    | Seq of policy * policy
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
    payload : payload
  }

  (** {2 Semantics}

      [eval pkt pol] raises [Not_found] if it tests or updates a header that  [pkt]
      does not have. This behavior is different from OpenFlow, which fails  silently
      in both cases.
  *)

  module PacketSet : Set.S
    with type elt = packet

  val eval : packet -> policy -> PacketSet.t

  (** {2 Utilities} *)

  val format_policy : Format.formatter -> policy -> unit

  val string_of_policy : policy -> string

  (** re-associates constructors so that pretty-printing produces as few
      parentheses as possible *)
  val pretty_assoc : policy -> policy

end

module Make : functor (Headers : HEADERS) -> S
  with type header = Headers.header
   and type header_val = Headers.value
   and type payload = Headers.payload
