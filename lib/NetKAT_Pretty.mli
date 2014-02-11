open NetKAT_Types

val format_policy : Format.formatter -> policy -> unit

val format_header : Format.formatter -> header -> unit

val header_to_string : header -> string

val string_of_field : SDN_Types.field -> string

val value_to_string : header_val -> string
  
val string_of_policy : policy -> string

val string_of_pred : pred -> string

val pretty_assoc : policy -> policy
