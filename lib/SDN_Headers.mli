(** {2 OpenFlow Headers} 

    We use the same packet headers that OpenFlow 1.0 supports. Refer to the
    OpenFlow specification for details on their semantics.

    The size of a header depends on the header name. But, insteam encoding this
    dependency in the type system, we place integers of various sizes in the
    [header_val] sum type.

    {b Warning:} Some header combinations are invalid and some headers
    depend on the presence of other headers. To be safe, use {i valid patterns}.

*)

type header = 
  | Header of SDN_Types.field
  | Switch

type value = VInt.t

type payload = SDN_Types.payload

val switch : header
val port : header
val vlan : header
val zero : value

val format_header : Format.formatter -> header -> unit

val format_value : Format.formatter -> value -> unit

val header_to_string : header -> string

val value_to_string : value -> string

val compare_header : header -> header -> int
